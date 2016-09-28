// Marek Sokolowski - Computer Network large assignment problem
// Player implementation (downloads radio stream from Internet along with metadata).
//
// Invocation:
//    ./player host path r-port file m-port md
// * host   - host from which stream is to be downloaded
// * path   - path in which one can find the stream
// * r-port - remote stream port
// * file   - file to save MP3 stream (dash '-' if put to stdout)
// * m-port - UDP port under which the player can be controlled
// * md     - 'no' or 'yes' depending on whether metadata should be downloaded and
//            parsed
//
// UDP commands:
//   PAUSE - pause downloading stream
//   PLAY  - resume downloading stream
//   TITLE - write current song name and performer
//   STOP  - cease downloading stream

#include <ctype.h>
#include <fcntl.h>
#include <netdb.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>

#include "err.h"
#include "fifo_wait.h"
#include "headers.h"
#include "icy.h"


// Ustawienia playera.
struct player_config {
  const char* progname;        // Nazwa programu.

  const char* host_name;       // Host, z ktorym sie laczymy.
  const char* host_path;       // Sciezka zasobu.
  bool is_remote;              // Czy zdalne uruchomienie?
  uint16_t host_port;          // Port, pod ktorym laczymy sie z hostem.
  int server_fd;               // Deskryptor serwera.
  pthread_t host_thread;       // Watek ssacy z hosta.
  
  struct fifo_wait fifo_wait;
  bool suspended;

  int output_fd;

  uint16_t listen_port;        // Port, na ktorym sluchamy pod UDP.
  int listen_fd;               // Deskryptor ...
  pthread_t listen_thread;     // Watek ...

  bool get_metadata;           // Czy bierzemy metadane?
  bool end_prog;               // Czy skonczyc program?

  int ret_code;                // Kod wyjscia.

  pthread_t beat_thread;       // Watek rzucajacy beaty keepalive.
} player_config;

struct icy_conn icy_conn;

#define PLAYER_ERROR(...) fprintf(stderr, "ERROR: " __VA_ARGS__)


__attribute__((noreturn))
static void fail_parameters(const char *progname) {
  PLAYER_ERROR("%s host path r-port file m-port md\n",
               progname ? progname : "./player");
  exit(1);
}


static uint16_t parse_port(const char *port_str) {
  char *badpos;
  long int port = strtol(port_str, &badpos, 10);
  if (port_str[0] == '\0' || *badpos != '\0' || port <= 0 || port > MAX_PORT) {
    PLAYER_ERROR("niepoprawny numer portu\n");
    exit(1);
  }

  return (uint16_t)port;
}


static void process_args(int argc, char **argv) {
  memset(&player_config, 0, sizeof(player_config));

  // Jesli odpowiednia zmienna srodowiskowa jest ustawiona, ustawiamy run_id.
  char *env = getenv(REMOTE_RUN);
  player_config.is_remote = (env != NULL);

  // Wyluskujemy kolejne parametry z argv.
  player_config.progname = argv[0];

  if (argc != 7) {
    fail_parameters(argv[0]);
  }

  player_config.host_name = argv[1];
  player_config.host_path = argv[2];

  player_config.host_port = parse_port(argv[3]);

  const char* filename = argv[4];
  if (!strcmp(filename, "-")) {
    // Nie mozna uzyc stdout przy zdalnym polaczeniu!
    if (player_config.is_remote) {
      PLAYER_ERROR("nie mozna wypisac strumienia na stdout\n");
      exit(1);
    }
    player_config.output_fd = STDOUT_FILENO;
  } else {
    player_config.output_fd = RUN_CMD(open, filename, O_CREAT | O_TRUNC | O_WRONLY, S_IRWXU);
  }

  player_config.listen_port = parse_port(argv[5]);

  const char* metadata = argv[6];
  if (!strcmp(metadata, "yes")) {
    player_config.get_metadata = true;
  } else if (!strcmp(metadata, "no")) {
    player_config.get_metadata = false;
  } else {
    PLAYER_ERROR("w parametrze 'md' oczekiwano 'yes' lub 'no'\n");
    exit(1);
  }

  player_config.end_prog = false;
}


static void setup_locks() {
  fifo_init(&player_config.fifo_wait);
}


// Funkcja finalizujaca; wywolywana pod koniec ktoregos watku, by zmusic wszystkie
// inne watki do zakonczenia.
static void finalize_threads(int errcode) {
  int oldstate, curstate;
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldstate);

  player_config.ret_code = errcode;
  player_config.end_prog = true;
  pthread_cancel(player_config.listen_thread);
  pthread_cancel(player_config.host_thread);
  if (player_config.is_remote) {
    pthread_cancel(player_config.beat_thread);
  }

  pthread_setcancelstate(oldstate, &curstate);
}


// Watek sluchajacy po UDP.
static void* listen_thread(void *param) {
#define MAX_LISTEN_BUFFER 1024
  (void)param;
  struct sockaddr_in client_address;
  char listen_buffer[MAX_LISTEN_BUFFER + 16];

  debug("I'm into listening thread!\n");
  while (!player_config.end_prog) {
    socklen_t rcva_len = (socklen_t)sizeof(client_address),
              snda_len = rcva_len;
    int flags = 0;
    int len = recvfrom(player_config.listen_fd, listen_buffer, MAX_LISTEN_BUFFER, flags,
                       (struct sockaddr*)&client_address, &rcva_len);
    
    if (len <= 0) {
      debug("Some error happened from client socket\n");
      continue;
    }

    listen_buffer[len] = 0;
    debug("Received message from client! (%s)\n", listen_buffer);

    if (!memcmp(listen_buffer, "PAUSE", 6)) {
      // Pauzujemy. Pauzowanie robimy, po prostu ustawiajac odpowiednia flage,
      // z ktorej potem korzystamy, komunikujac sie z icy.c.
      if (!player_config.suspended) {
        player_config.suspended = true;
        debug("Suspended!\n");
      }

    } else if (!memcmp(listen_buffer, "PLAY", 5)) {
      // Wznawiamy.
      if (player_config.suspended) {
        player_config.suspended = false;
        debug("Resumed!\n");
      }

    } else if (!memcmp(listen_buffer, "TITLE", 6)) {
      // Zwracamy tytul.
      int titlelen = icy_gettitle(listen_buffer, MAX_LISTEN_BUFFER);
      int sndlen = sendto(player_config.listen_fd, listen_buffer, (size_t)titlelen,
                          0, (struct sockaddr*)&client_address, snda_len);
      if (sndlen != titlelen) {
        debug("Error on sending back the message\n");
      }

    } else if (!memcmp(listen_buffer, "QUIT", 5)) {
      // Wychodzimy. Kod bledu - 0.
      finalize_threads(0);
      
    } else {
      debug("ERROR: incorrect UDP command!\n");

    }
  }

  return NULL;
#undef MAX_LISTEN_BUFFER
}


static void setup_listener() {
  // Ustawiamy socket na sluchanie po UDP.
  debug("Setting up UDP listener at (%d)...\n", player_config.listen_port);
  
  player_config.listen_fd = RUN_CMD(socket, AF_INET, SOCK_DGRAM, 0);
  int fd = player_config.listen_fd;
  static const int on_opt = 1;

  RUN_CMD(setsockopt, fd, SOL_SOCKET, SO_REUSEADDR, (char*)&on_opt, sizeof(on_opt));

  struct sockaddr_in address;
  memset(&address, 0, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_ANY);
  address.sin_port = htons(player_config.listen_port);
  RUN_CMD(bind, fd, (struct sockaddr*)&address, sizeof(address));

  // Tworzymy watek nasluchujacy.
  RUN_THREAD_CMD(pthread_create, &player_config.listen_thread, NULL,
                 listen_thread, NULL);
}


#define MAX_DATA 8192

// Watek laczacy sie z hostem.
static void* host_thread(void *param) {
  (void)param;
  debug("Host thread\n");

  char data_buf[MAX_DATA];

  // Ustawiamy polaczenie, czytamy headery.
  memset(&icy_conn, 0, sizeof(icy_conn));
  icy_conn.server_fd = player_config.server_fd;
  strncpy(icy_conn.src_path, player_config.host_path, ICY_MAX_PATH - 1);
  strncpy(icy_conn.src_host, player_config.host_name, ICY_MAX_PATH - 1);
  icy_conn.get_metadata = player_config.get_metadata;
  if (!icy_headers(&icy_conn)) {
    PLAYER_ERROR("Headers not read correctly\n");
    finalize_threads(1);
    return NULL;
  }

  
  while (!player_config.end_prog) {
    // Pobieramy dane (i ignorujemy je w przypadku wstrzymania).
    int nread = icy_getdata(data_buf, MAX_DATA, player_config.suspended);

    // Jesli nie wstrzymalismy, przetwarzamy dane.
    if (!player_config.suspended) {
      debug("icy_getdata returned %d\n", nread);
      if (nread == 0) {
        debug("Connection ended\n");
        finalize_threads(0);
      }
      if (nread < 0) {
        PLAYER_ERROR("Connection broken\n");
        finalize_threads(1);
      }

      // Po przetworzeniu ewentualnego EOF/error wypluwamy dane na deskryptor.
      char *data_ptr = data_buf;
      int towrite = nread;
      while (towrite > 0) {
        int x = write(player_config.output_fd, data_ptr, towrite);
        if (x <= 0) {
          PLAYER_ERROR("Something wrong happened with output file\n");
          finalize_threads(1);
          break;
        }

        towrite -= x;
      }

#ifdef DEBUG
      if (player_config.get_metadata) {
        towrite = icy_gettitle(data_buf, MAX_DATA);
        debug("title len: %d\n", towrite);
        debug("current title: ");
        for (int i = 0; i < towrite; i++) { debug("%c", data_buf[i]); }
        debug("\n");
      }
#endif
    }
  }

  return NULL;
}


// Laczenie sie z serwerem hosta.
static void connect_server() {
  debug("Connecting to server (%s) at (%d)...\n", player_config.host_name,
                                                  player_config.host_port);

  // Rozwiazujemy nazwe domenowa.
  struct addrinfo addr_hints, *addr_result;
  memset(&addr_hints, 0, sizeof(struct addrinfo));

  addr_hints.ai_family = AF_INET;
  addr_hints.ai_socktype = SOCK_STREAM;
  addr_hints.ai_protocol = IPPROTO_TCP;
  char port_data[10];
  memset(port_data, 0, 10);
  snprintf(port_data, 10, "%hu", player_config.host_port);
  int result = getaddrinfo(player_config.host_name, port_data, &addr_hints,
                           &addr_result);
  if (result == EAI_SYSTEM) {
    syserr("getaddrinfo");
  } else if (result != 0) {
    fatal("getaddrinfo: %s", gai_strerror(result));
  }

  // Tworzymy polaczenie po wskazanym porcie.
  player_config.server_fd = RUN_CMD(socket, addr_result->ai_family,
                                            addr_result->ai_socktype,
                                            addr_result->ai_protocol);
  RUN_CMD(connect_timeout, player_config.server_fd, addr_result->ai_addr,
                                                    addr_result->ai_addrlen);

  freeaddrinfo(addr_result);

  // Tworzymy nowy watek porozumiewajacy sie z serwerem.
  RUN_THREAD_CMD(pthread_create, &player_config.host_thread, NULL,
                 host_thread, NULL);
}



static void cleanup() {
  pthread_join(player_config.host_thread, NULL);
  pthread_join(player_config.listen_thread, NULL);
  if (player_config.is_remote) {
    pthread_join(player_config.beat_thread, NULL);
  }

  if (player_config.server_fd >= 0) {
    close(player_config.server_fd);
  }

  if (player_config.output_fd != STDOUT_FILENO) {
    close(player_config.output_fd);
  }

  fifo_destroy(&player_config.fifo_wait);
}


// Watek, ktory co 5 sekund wysyla krotki komunikat na stderr, zeby podtrzymac
// polaczenie SSH.
void *beat_thread(void *param) {
  (void)param;
  while (true) {
    sleep(5);
    fprintf(stderr, "beat\n");
  }
}



int main(int argc, char **argv) {
  process_args(argc, argv);

  setup_locks();
  setup_listener();
  connect_server();

  if (player_config.is_remote) {
    // Jesli remote, to co piec sekund puszczamy beat na stderr.
    RUN_THREAD_CMD(pthread_create, &player_config.beat_thread, NULL,
                   beat_thread, NULL);
  }

  cleanup();

  return player_config.ret_code;
}
