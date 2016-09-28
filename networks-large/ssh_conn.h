// Marek Sokolowski - Computer Network large assignment problem
// Subprogram setting up and maintaining SSH connection with playing computers.
// Uses libssh2.
#ifndef __SSH_CONN
#define __SSH_CONN

#include <stdbool.h>

#include <netdb.h>
#include <libssh2.h>

#include <sys/types.h>
#include <sys/socket.h>

#include "fifo_wait.h"
#include "utils.h"


#define SSH_MAX_HOST_SIZE 256
#define SSH_MAX_NAME_SIZE 256
#define SSH_MAX_ARGS_SIZE 1024

// Konfiguracja polaczenia ssh.
struct ssh_conn_config {
  LIBSSH2_SESSION *session;   // Zmienne libssh2.
  LIBSSH2_CHANNEL *channel;

  char host_name[SSH_MAX_HOST_SIZE];   // Nazwa hosta, argumenty wywolania.
  char player_args[SSH_MAX_ARGS_SIZE];
  char player_invoc[SSH_MAX_ARGS_SIZE + 128]; // Wywolanie.
  char username[SSH_MAX_NAME_SIZE];    // Nazwa aktualnego uzytkownika.

  bool terminate, started;   // Czy skonczyc polaczenie? Czy zaczelo sie?
  struct fifo_wait wait;     // Synchronizacja watkow (potrzebna, by zakonczenie sie nie psulo).
  struct safe_fifo *terms;   // Kolejka zakonczen; wspolna dla wszystkich

  int host_fd;               // Deskryptor polaczenia z serwerem.
  int client_id;             // Moje ID.

  FILEBUF *telnet_buf;       // Buforowane polaczenie z telnetem.
  struct sockaddr_in ssh_addr; // Adres, pod ktorym laczymy sie po SSH.
  int udp_port;              // Port UDP.
  pthread_t thread;          // Watek SSH.
  pthread_mutex_t *read_mutex;  // Globalny muteks chroniacy przed wspolbieznym czytaniem po
                                // libssh2.
};


// Ustaw dane do polaczenia w SSH. Zwroc, czy sie udalo.
bool player_setup_data(struct ssh_conn_config *config);

// Polacz sie z serwerem i uruchom playera. Zwroc, czy sie udalo.
bool player_run_ssh(struct ssh_conn_config *config);

// Zatrzymaj playera.
void player_stop_ssh(struct ssh_conn_config *config);


#endif
