// Marek Sokolowski - Computer Network large assignment problem
// Subprogram setting up and maintaining SSH connection with playing computers.
// Uses libssh2.
#include "ssh_conn.h"
#include "headers.h"
#include "utils.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// Czyszczenie sesji.
static void ssh_conn_cleanup(void *param) {
  struct ssh_conn_config *ssh = (struct ssh_conn_config*)param;
  debug("Cleanup!\n");

  libssh2_channel_close(ssh->channel);
  libssh2_channel_free(ssh->channel);
  libssh2_session_disconnect(ssh->session, "");
  libssh2_session_free(ssh->session);

  // Powiadamiamy innych o koncu swojego zywota.
  safe_fifo_push_back(ssh->terms, NULL, ssh->client_id);
}


static void *ssh_conn_thread(void *param) {
#define SSH_BUFFER_SIZE 2048
#define SSH_TIMEOUT (10 * 1000)
  // Ustawiamy handlera anulowania naszego watku.
  pthread_cleanup_push(ssh_conn_cleanup, param);

  struct ssh_conn_config *ssh = (struct ssh_conn_config*)param;
  ssh->started = true;

  // Teraz juz nie blokujemy.
  libssh2_session_set_blocking(ssh->session, 0);

  struct pollfd fds;
  fds.fd = ssh->host_fd;
  fds.events = POLLIN;

  // 'buffer' trzyma informacje o obecnej linijce stderr (byc moze niedokonczonej).
  char buffer[SSH_BUFFER_SIZE + 1];
  memset(buffer, 0, SSH_BUFFER_SIZE + 1);
  int buffer_pos = 0;

  while (true) {
    // Czekamy na event; jakis powinien sie zdarzyc w ciagu 10 sekund, w szczegolnosci,
    // ze zdalnie odpalony player wysyla heartbeaty.
    int nfds = poll(&fds, 1, SSH_TIMEOUT);
    debug("%d %d\n", fds.events, fds.revents);
    if (nfds <= 0) {
      utils_block_printf(ssh->telnet_buf, "ERROR %d No response from client\n", ssh->client_id);
      break;
    }
    if (fds.revents & (POLLERR | POLLHUP)) {
      utils_block_printf(ssh->telnet_buf, "ERROR %d Error on SSH descriptor\n", ssh->client_id);
      break;
    }

    // Czytamy dane; tu wazne, by nie bylo wielu procesow naraz!
    fifo_enter(&ssh->wait);
    if (ssh->terminate) { fifo_leave(&ssh->wait); break; }

    pthread_mutex_lock(ssh->read_mutex);
    // doklejamy stderr na koniec bufora.
    int rc = libssh2_channel_read_stderr(ssh->channel, buffer + buffer_pos,
                                         SSH_BUFFER_SIZE - buffer_pos);
    pthread_mutex_unlock(ssh->read_mutex);

    if (rc > 0) {
      debug("Got %d bytes from server\n", rc);
      int lptr = 0;
      buffer_pos += rc;
      bool end_prog = false;
      buffer[buffer_pos] = 0;

      for (int i = 0; i < buffer_pos; i++) {
        // Sprawdzamy kolejne linie; jesli ktoras zaczyna sie bajtami 'ERROR:', wypisujemy
        // na telneta blad, a nastepnie konczymy polaczenie.
        if (buffer[i] == '\n') {
          if (is_prefix(buffer + lptr, "ERROR:")) {
            utils_block_printf(ssh->telnet_buf, "ERROR %d%*s",
                               ssh->client_id, i - lptr - 6, buffer + lptr + 6);
            end_prog = true;
          }
        }
      }
      if (end_prog) {
        fifo_leave(&ssh->wait);
        break;
      }

      // Przesuwamy bufer w lewo, na wszystkie wczytane linie.
      for (int i = lptr; i < buffer_pos; i++) {
        buffer[i - lptr] = buffer[i];
      }

      buffer_pos -= lptr;
      if (buffer_pos == SSH_BUFFER_SIZE) {
        // Jesli przeczytalismy SSH_BUFFER_SIZE bajtow bez konca linii, dropimy je.
        buffer_pos = 0;
      }

    } else if (rc == 0) {
      debug("Connection ended gracefully\n");
      fifo_leave(&ssh->wait);
      break;
    } else if (rc == LIBSSH2_ERROR_EAGAIN) {
      debug("No data for now\n");
    } else {
      char *rcptr;
      libssh2_session_last_error(ssh->session, &rcptr, NULL, 0);
      utils_block_printf(ssh->telnet_buf, "ERROR %d SSH: %s\n", ssh->client_id, rcptr);

      fifo_leave(&ssh->wait);
      break;
    }
    fifo_leave(&ssh->wait);
  }
  
  pthread_cleanup_pop(true);
  return NULL;

#undef SSH_BUFFER_SIZE
#undef SSH_TIMEOUT
}



// Duzo razy bedziemy korzystali z funkcji "jesli nie wyszlo, to wypluj blad na telneta
// i zwroc 'false' / skocz do etykiety". Te makra pomagaja nam w tym.
#define ERROR_FALSE(who) { \
  utils_block_printf(ssh->telnet_buf, "ERROR " who ": %s\n", \
                     strerror(errno)); \
  return false; \
}

#define CHECK_SSH2_ERROR(fail, who, label) { \
  if (fail) { \
    char *rcptr; \
    libssh2_session_last_error(ssh->session, &rcptr, NULL, 0); \
    utils_block_printf(ssh->telnet_buf, "ERROR " who ": %s\n", \
                       rcptr); \
    goto label; \
  } \
}
  

bool player_setup_data(struct ssh_conn_config *ssh) {
  // Budujemy programowi parametry.
  const char forbidden_chars[10] = "|&\\\'\"\r\n\t;";
  for (int i = 0; i < 9; i++) {
    if (strchr(ssh->player_args, forbidden_chars[i])) {
      utils_block_printf(ssh->telnet_buf, "ERROR unsanitized input\n");
      return false;
    }
  }

  strcpy(ssh->player_invoc, "bash -l -c 'export " REMOTE_RUN "=1 && player ");
  strncat(ssh->player_invoc, ssh->player_args, SSH_MAX_ARGS_SIZE);
  strncat(ssh->player_invoc, "'", SSH_MAX_ARGS_SIZE);

  // Pobieramy nazwe uzytkownika.
  int rc = getlogin_r(ssh->username, 256);
  if (rc < 0) {
    ERROR_FALSE("getlogin_r");
  }
  debug("Will be connecting with username %s\n", ssh->username);

  // Uzyskujemy numer portu.
  char *args_copy = strndup(ssh->player_args, SSH_MAX_ARGS_SIZE);
  char **args_tokens = parse_into_tokens(args_copy);
  // Ma byc dokladnie szesc argumentow; piaty to numer portu.
  for (int i = 0; i < 6; i++) {
    if (!args_tokens[i]) {
      utils_block_printf(ssh->telnet_buf, "ERROR too few args\n");
      return false;
    }
  }
  if (args_tokens[6]) {
    utils_block_printf(ssh->telnet_buf, "ERROR too many args\n");
    return false;
  }

  char *badptr;
  long udp_port = strtol(args_tokens[4], &badptr, 10);
  if (!args_tokens[4][0] || *badptr || udp_port <= 0 || udp_port > MAX_PORT) {
    utils_block_printf(ssh->telnet_buf, "ERROR incorrect port\n");
    return false;
  }
  debug("got port %ld\n", udp_port);
  ssh->udp_port = udp_port;
  debug("(%p)\n", ssh);

  free(args_copy);
  for (char **ptr = args_tokens; *ptr; ptr++) {
    free(*ptr);
  }
  free(args_tokens);


  return true;
}


bool player_run_ssh(struct ssh_conn_config *ssh) {
  // Rozwiazujemy nazwe hosta i nawiazujemy polaczenie TCP.
  struct addrinfo addr_hints, *addr_result;
  memset(&addr_hints, 0, sizeof(struct addrinfo));

  addr_hints.ai_family = AF_INET;
  addr_hints.ai_socktype = SOCK_STREAM;
  addr_hints.ai_protocol = IPPROTO_TCP;
  const char *port_data = "22";

  int aresult = getaddrinfo(ssh->host_name, port_data, &addr_hints, &addr_result);
  if (aresult == EAI_SYSTEM) {
    ERROR_FALSE("getaddrinfo");
  } else if (aresult != 0) {
    utils_block_printf(ssh->telnet_buf, "ERROR getaddrinfo: %s\n", gai_strerror(aresult));
    return false;
  }

  ssh->host_fd = socket(addr_result->ai_family,
                        addr_result->ai_socktype,
                        addr_result->ai_protocol);
  if (ssh->host_fd < 0) {
    ERROR_FALSE("socket");
    return false;
  }

  // Laczymy z timeoutem.
  if (connect_timeout(ssh->host_fd, addr_result->ai_addr, addr_result->ai_addrlen) < 0) {
    ERROR_FALSE("connect_timeout");
  }

  memcpy(&ssh->ssh_addr, addr_result->ai_addr, sizeof(*addr_result->ai_addr));
  freeaddrinfo(addr_result);


  pthread_mutex_lock(ssh->read_mutex);

  // Budujemy sesje SSH.
  ssh->session = libssh2_session_init();
  if (!ssh->session) {
    utils_block_printf(ssh->telnet_buf, "ERROR libssh2_session_init failed\n");
    goto unlock_mutex;
  }

  // Wymieniamy informacje o polaczeniu.
  int rc = libssh2_session_handshake(ssh->session, ssh->host_fd);
  CHECK_SSH2_ERROR(rc, "libssh2_session_handshake", destroy_session);

  // Strzelamy, gdzie jest klucz publiczny i prywatny - sa konieczne do polaczenia
  // bez hasla.
#define MAX_HOME_LEN 200
  char rsa_priv_loc[256], rsa_pub_loc[256];
  char *hdir = getenv("HOME");
  if (!hdir || strlen(hdir) > MAX_HOME_LEN) {
    utils_block_printf(ssh->telnet_buf, "ERROR home directory not found or too long\n");
    goto destroy_session;
  }
#undef MAX_HOME_LEN

  strcpy(rsa_priv_loc, hdir);
  strcat(rsa_priv_loc, "/.ssh/id_rsa");
  strcpy(rsa_pub_loc, rsa_priv_loc);
  strcat(rsa_pub_loc, ".pub");

  // Probujemy polaczyc sie kluczem publicznym.
  rc = libssh2_userauth_publickey_fromfile(ssh->session, ssh->username,
                                           rsa_pub_loc,
                                           rsa_priv_loc, "");
  CHECK_SSH2_ERROR(rc, "libssh2_userauth_publickey_fromfile", destroy_session);

  // Otwieramy sesje.
  ssh->channel = libssh2_channel_open_session(ssh->session);
  CHECK_SSH2_ERROR(!ssh->channel, "libssh2_channel_open_session", destroy_session);

  // Tworzymy bezpieczna kolejke dostepu.
  fifo_init(&ssh->wait);
  
  // Uruchamiamy program.
  rc = libssh2_channel_exec(ssh->channel, ssh->player_invoc);
  CHECK_SSH2_ERROR(rc, "libssh2_channel_exec", destroy_channel);

  // Ustawiamy na kazda operacje limit 10s.
  libssh2_session_set_timeout(ssh->session, 10);
  
  // Uruchamiamy watek obslugujacy polaczenie.
  rc = pthread_create(&ssh->thread, NULL, ssh_conn_thread, ssh);
  if (rc < 0) {
    utils_block_printf(ssh->telnet_buf, "ERROR pthread_create: %s\n", strerror(errno));
    goto destroy_conn;
  }
  
  // Zwalniamy muteks dzialania na polaczeniu.
  pthread_mutex_unlock(ssh->read_mutex);
  return true;

  // Zwalnianie zasobow, gdy sie nie udalo.
  destroy_conn:
  libssh2_channel_close(ssh->channel);

  destroy_channel:
  libssh2_channel_free(ssh->channel);

  destroy_session:
  libssh2_session_disconnect(ssh->session, "");
  libssh2_session_free(ssh->session);

  unlock_mutex:
  pthread_mutex_unlock(ssh->read_mutex);

  return false;
}


// Zatrzymaj playera.
void player_stop_ssh(struct ssh_conn_config *config) {
  // Ustawiamy bezpiecznie flage 'terminate' na 1.
  fifo_enter(&config->wait);
  debug("%p\n", &config->terminate);
  config->terminate = 1;
  debug("Terminating\n");
  fifo_leave(&config->wait);

  // Anulujemy i laczymy sie z watkiem SSH.
  pthread_cancel(config->thread);
  pthread_join(config->thread, NULL);
}
