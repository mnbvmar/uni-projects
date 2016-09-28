// Marek Sokolowski - Computer Network large assignment problem
// Master implementation (can control players over network).
//
// Invocation:
//   ./master [port-num]
//
// If port-num is given, sets up telnet server at this port; else seeks for a free
// port and sets up server there.
//
// Telnet commands:
//   START location args...     - start player at given network location (using SSH)
//                                   using args; id of player is returned
//   AT hh.mm location args...  - start player at given local hour and network location;
//                                   id of player is returned on success
//   PAUSE id, PLAY id, STOP id - pause/play/stop player instance #id (UDP command)
//   TITLE id                   - get current title in instance #id and print to telnet
//
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <poll.h>
#include <pthread.h>
#include <unistd.h>

#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "at.h"
#include "err.h"
#include "headers.h"
#include "fifo_wait.h"
#include "ssh_conn.h"
#include "utils.h"

struct master_config {            // Konfiguracja mastera.
  uint16_t telnet_port;           // Port nasluchujacy.
  int listen_socket;              // Gniazdo nasluchujace.
  struct safe_fifo telnet_conns;  // Kolejka informacji o polaczeniach telnet.
  struct safe_fifo player_conns;  // Kolejka informacji o playerach (SSH).
  struct safe_fifo player_terms;  // Kolejka zdarzen - zakonczen procesu
  pthread_mutex_t ssh_read_mutex; // Mutex na (nieblokujacy) odczyt po SSH.
  pthread_t cleaner_thread;       // Watek czyszczacy playery.
} master_config;


struct telnet_conn_config {       // Konfiguracja telneta.
  pthread_t telnet_thread;        // Watek sesji telnet.
  int telnet_fd, udp_fd;          // Deskryptor sesji telnet oraz socketa UDP.
  int udp_port;                   // Port, po ktorym sluchamy na UDP.
};


struct player_conn_config {       // Konfiguracja playera.
  int telnet_fd;                  // Deskryptor sesji telnet.
  struct ssh_conn_config ssh;     // Polaczenie ssh.
};




__attribute__((noreturn))
static void fail_parameters(const char *progname) {
  fprintf(stderr, "ERROR: %s [telnet_port]\n", progname ? progname : "./master");
  exit(1);
}

static void process_args(int argc, char **argv) {
  if (argc != 1 && argc != 2) { fail_parameters(argv[0]); }

  if (argc == 2) {
    char *badpos;
    long port = strtol(argv[1], &badpos, 10);
    if (*argv[1] == '\0' || *badpos != '\0' || port <= 0 || port > MAX_PORT) {
      fail_parameters(argv[0]);
    }
    master_config.telnet_port = port;
  } else {
    master_config.telnet_port = 0;   // 0 - chce dostac jakis port.
  }
}


static void setup_telnet() {
  debug("Setting up telnet listener\n");

  // Ustawiamy polaczenie telneta.
  int on_opt = 1;
  int listen_socket = RUN_CMD(socket, AF_INET, SOCK_STREAM, 0);
  RUN_CMD(setsockopt, listen_socket, SOL_SOCKET, SO_REUSEADDR, (char*)&on_opt, sizeof(on_opt));

  struct sockaddr_in address;
  memset(&address, 0, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_ANY);

  // Jesli 0, to chce dostac od systemu jakis port, a potem go wypisac na stdout.
  bool show_port = (master_config.telnet_port == 0);
  address.sin_port = htons(master_config.telnet_port);
  RUN_CMD(bind, listen_socket, (struct sockaddr*)&address, sizeof(address));

  if (show_port) {
    // Uzyskuje numer portu, zapisuje go sobie i wypisuje uzytkownikowi.
    socklen_t addrlen = sizeof(struct sockaddr_in);
    RUN_CMD(getsockname, listen_socket, (struct sockaddr*)&address, &addrlen);
    master_config.telnet_port = ntohs(address.sin_port);
    printf("%d\n", master_config.telnet_port);
  }

  RUN_CMD(listen, listen_socket, 10);

  master_config.listen_socket = listen_socket;
  safe_fifo_init(&master_config.telnet_conns);
}


// Wlasciwe uruchomienie playera.
static void start_player(int id) {
  if (id == -1) { return; }

  // Dostajemy z kolejki informacje o playerze.
  struct safe_fifo_elem *elem = safe_fifo_front_by_id(&master_config.player_conns, id);
  if (!elem) {
    debug("Weird, player should be existent\n");
    return;
  }
  struct player_conn_config *conf = (struct player_conn_config*)(elem->elem);

  // Uruchamiamy przez SSH; jesli sie nie uda, wyrzucamy z kolejki
  if (!player_run_ssh(&conf->ssh)) {
    safe_fifo_erase(&master_config.player_conns,
                    safe_fifo_front_by_id(&master_config.player_conns, id));
  }
}


// Ustawiamy playera (ale go nie uruchamiamy).
static int setup_player(const char *loc, const char *args, int client_fd) {
  debug("Setting up player at [%s] with args [%s]...\n", loc, args);

  // Tworzymy i wypelniamy informacje playera.
  struct player_conn_config *conf = calloc(sizeof(struct player_conn_config), 1);
  conf->telnet_fd = client_fd;
  conf->ssh.telnet_buf = utils_get_fd(client_fd);

  debug("got (%p)\n", conf->ssh.telnet_buf);
  conf->ssh.read_mutex = &master_config.ssh_read_mutex;    // Mutex na czytanie globalny!
  conf->ssh.terms = &master_config.player_terms;

  strncpy(conf->ssh.host_name, loc, SSH_MAX_HOST_SIZE - 1);
  strncpy(conf->ssh.player_args, args, SSH_MAX_ARGS_SIZE - 1);

  // Jesli nie przejdzie ustawianie danych w SSH, wypluwamy -1
  if (!player_setup_data(&conf->ssh)) { return -1; }

  // Wrzucamy na kolejke playera, stad mamy jego id.
  int id = safe_fifo_push_back(&master_config.player_conns, conf, -1);
  conf->ssh.client_id = id;

  debug("%d\n", id);
  utils_block_printf(conf->ssh.telnet_buf, "OK %d\n", id);

  return id;
}


// Akcja wykonywana przy poleceniu AT polegajaca na uruchomieniu playera o ID.
static void at_start_player(union sigval sigval) {
  start_player(sigval.sival_int);
}

// Akcja przy poleceniu AT - zabij playera o danym ID.
static void at_end_player(union sigval sigval) {
  int id = sigval.sival_int;

  // Znajdujemy watek ssh obslugujacy playera i go anulujemy i joinujemy.
  struct safe_fifo_elem *elem = safe_fifo_front_by_id(&master_config.player_conns, id);
  if (!elem) {
    debug("Wanted to cease the player's existence, but it commited suicide earlier...\n");
    return;
  }

  struct player_conn_config *conf = (struct player_conn_config*)(elem->elem);
  pthread_t *thread = &conf->ssh.thread;
  pthread_cancel(*thread);
  pthread_join(*thread, NULL);
}


#define UDP_PORT_BASE 34312

// Wyslij operacje op (juz sprawdzona, ze jest ok) do playera player_id.
// Socket UDP to fd, bufor telneta to telnet.
static void send_op_to_player(char *op, int player_id, int fd, FILEBUF *telnet) {
  // Pobieramy informacje o polaczeniu, w szczegolnosci wyluskujemy hosta
  // i port UDP.
  struct safe_fifo_elem *elem = safe_fifo_front_by_id(&master_config.player_conns, player_id);
  if (!elem) {
    utils_block_printf(telnet, "ERROR No player id %d\n", player_id);
    return;
  }

  struct player_conn_config *player = (struct player_conn_config*)(elem->elem);
  struct ssh_conn_config *ssh = &player->ssh;
  if (!ssh->started) {
    utils_block_printf(telnet, "ERROR Player %d is not active yet\n", player_id);
    return;
  }

  struct sockaddr_in *addr = &ssh->ssh_addr;
  addr->sin_port = htons(ssh->udp_port);
  debug("PORT = %d\n", ssh->udp_port);

  // Wyslij datagram UDP.
  sendto(fd, op, strlen(op), 0,
         (struct sockaddr*)addr, (socklen_t)sizeof(*addr));

  //utils_block_printf(telnet, "OK %s sent to %d\n", op, player_id);

  // Jesli TITLE, to musimy poznac odpowiedz.
  if (!strcmp(op, "TITLE")) {
    struct pollfd udp_poll;
    udp_poll.fd = fd;
    udp_poll.events = POLLIN;
    int res = poll(&udp_poll, 1, 3 * 1000);   // Timeout 3 sekundy na odpowiedz.

    if (res == 0) {
      utils_block_printf(telnet, "ERROR %d No response in 3 seconds\n", player_id);
    } else if (udp_poll.revents & (POLLERR | POLLHUP)) {
      utils_block_printf(telnet, "ERROR %d Some error on UDP descriptor\n", player_id);
    } else {
      char buf[256];
      ssize_t len = recv(fd, buf, 250, 0);
      if (len < 0) {
        utils_block_printf(telnet, "ERROR %d UDP error occured\n", player_id);
      } else {
        buf[len] = 0;
        utils_block_printf(telnet, "OK %d %s\n", player_id, buf);
      }
    }
  } else {
    utils_block_printf(telnet, "OK %d\n", player_id);
  }
}


// Majac polaczenie SSH, wyrzuc z niego wskazany deskryptor telneta, o ile ten
// jest wpisany. (Ma to sens, gdy sie rozlaczamy i juz pisanie na telneta nie ma
// sensu).
static void remove_telnet_fds(void *ssh_void, void *param) {
  int fd = *(int*)param;
  struct player_conn_config *player = (struct player_conn_config*)ssh_void;
  struct ssh_conn_config *ssh = &player->ssh;

  utils_erase_fd(ssh->telnet_buf, fd);
}

// Czysczenie sesji telneta.
static void cleanup_telnet(int telnet_fd) {
  // Wyrzucamy deskryptory 'telnet_fd' ze wszystkich playerow.
  safe_fifo_apply(&master_config.player_conns, remove_telnet_fds, &telnet_fd);
  // Wyrzucamy telneta z kolejki.
  struct safe_fifo_elem *elem = safe_fifo_front_by_id(&master_config.telnet_conns, telnet_fd);
  safe_fifo_erase(&master_config.telnet_conns, elem);
  close(telnet_fd);
}


// Watek z polaczeniem telnetowym.
static void* telnet_connection(void *param) {
#define MAX_TELNET_LINE 1024
  struct telnet_conn_config *telnet = (struct telnet_conn_config*)param;
  int client_fd = telnet->telnet_fd;
  FILEBUF *file = utils_get_fd(client_fd);

  char telnet_line[MAX_TELNET_LINE];
  ssize_t linesize;
  const char* delimiters = " \r\n";

  // Uzyskujemy UDP.
  int udp_fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (udp_fd < 0) {
    debug("Could not make UDP socket for connection: %s", strerror(errno));
    utils_block_printf(file, "ERROR Could not setup UDP in connection: %s\n", strerror(errno));
    goto telnet_cleanup;
  }

  struct sockaddr_in address;
  memset(&address, 0, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_ANY);
  address.sin_port = htons(0);

  if (bind(udp_fd, (struct sockaddr*)&address, sizeof(address))) {
    debug("Could not bind UDP socket for telnet: %s\n", strerror(errno));
    utils_block_printf(file, "ERROR Could not setup connection\n");
    goto telnet_cleanup;
  }

  // Zgadujemy teraz numer portu
  socklen_t addrlen = sizeof(address);
  if (getsockname(udp_fd, (struct sockaddr*)&address, &addrlen)) {
    debug("Could not get info about UDP socket: %s\n", strerror(errno));
    utils_block_printf(file, "ERROR Could not setup connection\n");
    goto telnet_cleanup;
  }
  telnet->udp_port = ntohs(address.sin_port);
  telnet->udp_fd = udp_fd;
  debug("Set up UDP listener at (%d)\n", listen_port);


  // Pobieramy kolejne linie z polaczenia telnet.
  while ((linesize = utils_getline(file, telnet_line, MAX_TELNET_LINE, true)) != FILEBUF_EOF) {
    if (linesize < 0) {
      debug("Some error on telnet site, need to disconnect\n");
      break;
    }

    debug("%zd\n", linesize);
    char *line = telnet_line;
    char *cmd = parse_token(&line, delimiters);

    if (!cmd) { continue; } // Pusta linia.
    //debug("%s\n", cmd);

    // Polecenie START loc ...
    if (!strcmp(cmd, "START")) {
      // Bierzemy pierwszy token; pozostala czesc linii jest w line.
      char *loc = parse_token(&line, delimiters);
      if (!loc) {
        utils_block_printf(file, "ERROR No location given in START command\n");
        continue;
      }

      // Ustawiamy playera, jak sie uda, to go startujemy.
      start_player(setup_player(loc, line, client_fd));

    } else if (!strcmp(cmd, "AT")) {
      // Polecenie AT hh.mm MM loc ...
      // Parsujemy kolejne tokeny, przetwarzamy je i sprawdzamy ich poprawnosc.
      char *tm_start_str = parse_token(&line, delimiters);
      char *mins_run_str = parse_token(&line, delimiters);
      char *loc = parse_token(&line, delimiters);
      char *badloc;

      // Co moze byc zle? Moze byc za malo tokenow...
      if (!tm_start_str || !mins_run_str || !loc) {
        utils_block_printf(file, "ERROR AT command too short\n");
        continue;
      }

      // ...minuty moga nie byc poprawna liczba...
      long mins_run = strtol(mins_run_str, &badloc, 10);
      if (!*mins_run_str || *badloc || mins_run <= 0 || mins_run > 1e7) {
        utils_block_printf(file, "ERROR Incorrect duration time in AT command\n");
        continue;
      }

      // ... hh.mm moze nie byc poprawna godzina...
      int secs_start = parse_time(tm_start_str);
      if (secs_start == -1) {
        utils_block_printf(file, "ERROR Incorrect start time\n");
        continue;
      }

      // ...moze sie nie udac ustawic playera...
      int id = setup_player(loc, line, client_fd);
      if (id == -1) {
        continue;
      }

      // ...moze sie nie udac ustawic timera zakonczenia...
      if (!do_in_time(secs_start + mins_run * 60, at_end_player, id)) {
        utils_block_printf(file, "ERROR Could not set up player end\n");
        continue;
      }

      // ...lub timera startu.
      if (!do_in_time(secs_start, at_start_player, id)) {
        utils_block_printf(file, "ERROR Could not set up player start\n");
        continue;
      }

    // Komendy UDP. (cmd num)
    } else if (!strcmp(cmd, "PAUSE") || !strcmp(cmd, "PLAY") ||
               !strcmp(cmd, "TITLE") || !strcmp(cmd, "QUIT")) {
      // Parsujemy *liczbe* - id playera. Potem ma nic nie byc.
      char *num = parse_token(&line, delimiters);
      if (!num) {
        utils_block_printf(file, "ERROR No player id given in %s command\n", cmd);
        continue;
      }

      if (parse_token(&line, delimiters)) {
        utils_block_printf(file, "ERROR Too long %s command\n", cmd);
        continue;
      }

      char *badpos;
      long player_id = strtol(num, &badpos, 10);
      if (*badpos) {
        utils_block_printf(file, "ERROR Invalid player id format in %s command\n", cmd);
        continue;
      }

      // Jak sie udalo, wysylamy UDP do playera.
      send_op_to_player(cmd, player_id, telnet->udp_fd, file);

    } else {
      // Nie trafiles z komenda!
      utils_block_printf(file, "ERROR Unrecognized command\n");
    }
  }

  telnet_cleanup:

  // Handler czyszczacy sesje telneta.
  close(telnet->udp_fd);
  cleanup_telnet(client_fd);
  utils_free_buf(file);

  return NULL;
#undef MAX_TELNET_LINE
}



// Watek czyszczacy playery.
static void* cleaner_thread(void *param) {
  (void)param;

  while (true) {
    // Czekamy na kolejce na zdarzenia czyszczenia playerow (same nie umieja sie
    // wyczyscic).
    safe_fifo_wait_for_elem(&master_config.player_terms);
    struct safe_fifo_elem *elem = safe_fifo_front(&master_config.player_terms);
    int id = elem->id;
    // Usuwamy info o zakoczeniu z kolejki - mamy juz id.
    safe_fifo_erase(&master_config.player_terms, elem);

    debug("Cleaning player with id %d\n", id);

    // Bierzemy playera.
    struct safe_fifo_elem *player_elem = safe_fifo_front_by_id(&master_config.player_conns, id);
    if (!player_elem) {
      debug("Weird, player with id %d not found\n", id);
      continue;
    }

    // Niszczymy bufor telneta dla tego playera.
    struct player_conn_config *player = (struct player_conn_config*)player_elem->elem;
    struct ssh_conn_config *ssh = &player->ssh;
    utils_block_printf(ssh->telnet_buf, "Player id %d ended\n", id);
    utils_free_buf(ssh->telnet_buf);

    // Wyrzucamy playera z kolejki.
    safe_fifo_erase(&master_config.player_conns, player_elem);
  }

  return NULL;
}


static void run_cleaner() {
  safe_fifo_init(&master_config.player_terms);
  RUN_THREAD_CMD(pthread_create, &master_config.cleaner_thread, NULL, cleaner_thread, NULL);
}


static void run_server() {
  // Ustawiamy odpowiednie kolejki i muteksy.
  safe_fifo_init(&master_config.telnet_conns);
  safe_fifo_init(&master_config.player_conns);
  pthread_mutex_init(&master_config.ssh_read_mutex, NULL);

  while (true) {
    // Czekamy (blokujaco, jestesmy w koncu oddzielnym watkiem!) na polaczenia telneta.
    debug("Waiting on acceptance\n");
    int new_socket = accept(master_config.listen_socket, NULL, NULL);
    struct telnet_conn_config *conn = calloc(sizeof(struct telnet_conn_config), 1);
    conn->telnet_fd = new_socket;

    debug("my id = %d\n", new_socket);
    safe_fifo_push_back(&master_config.telnet_conns, conn, new_socket);

    // Mamy nowego ochotnika, tworzymy mu nowy watek.
    RUN_THREAD_CMD(pthread_create, &conn->telnet_thread, NULL, telnet_connection, conn);
  }

  safe_fifo_destroy(&master_config.player_conns);
  safe_fifo_destroy(&master_config.telnet_conns);
  pthread_mutex_destroy(&master_config.ssh_read_mutex);
}


int main(int argc, char **argv) {
  srand((unsigned)time(NULL));
  process_args(argc, argv);
  setup_telnet();
  run_cleaner();
  run_server();
}
