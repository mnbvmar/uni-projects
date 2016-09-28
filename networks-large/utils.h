// Marek Sokolowski - Computer Network large assignment problem
// Player/master utilities - own pollable 'mutexable' timeoutable file descriptor,
// simple string operations and timeoutable connection.
#ifndef __UTILS
#define __UTILS

#include <stdbool.h>
#include <stdio.h>

#include <sys/socket.h>

#define FILEBUF_EOF (-15)

// Struktura buforujaca deskryptor.
struct FILEBUF;
typedef struct FILEBUF FILEBUF;

// Ustawia aktualnie przetwarzany deskryptor na inny; moze spowodowac utrate danych
// z innego deskryptora.
FILEBUF *utils_get_fd(int fd);

// Wywala powiazanie z deskryptorem. Kazde nastepne uzycie FILEBUF nie zrobi nic.
void utils_erase_fd(FILEBUF *file, int fd);

// Ustawia timeout, po ktorym zwrocone zostanie 0.
void utils_set_timeout(FILEBUF *file, int timeout);

// Blokujacy write.
ssize_t utils_block_write(FILEBUF *file, void *buf, size_t count);

// Blokujacy printf.
ssize_t utils_block_printf(FILEBUF *file, const char *format, ...)
  __attribute__((format (printf, 2, 3)));

// Przeczytanie pojedynczego znaku. W przypadku EOF -1, w przypadku bledu -2.
int utils_readchar(FILEBUF *file);

// Wersja getline, konczaca sie na '\n' (lub '\n'|'\r'|'\0', gdy do_telnet = true).
ssize_t utils_getline(FILEBUF *file, char *lineptr, int max_line, bool do_telnet);

// Dokladnie read (poza tym, ze fd ustalone) i sa timeouty.
ssize_t utils_read(FILEBUF *file, void *buf, size_t count);

// Zwalnia bufor.
void utils_free_buf(FILEBUF *buf);


// INNE

// Czy jakis pattern jest prefiksem tekstu?
bool is_prefix(const char *text, const char *pattern);

// Sparsuj tekst w ciag tokenow
char** parse_into_tokens(const char *text);

// Sparsuj pojedynczy token; w **text zwroc wskaznik za tokenem.
char* parse_token(char **text, const char *delims);

// Polacz sie z timeoutem rownym 5 sekund.
int connect_timeout(int fd, const struct sockaddr *addr, socklen_t addrlen);

#endif
