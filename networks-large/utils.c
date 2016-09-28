// Marek Sokolowski - Computer Network large assignment problem
// Player/master utilities - own pollable 'mutexable' timeoutable file descriptor,
// simple string operations and timeoutable connection.
#include "headers.h"
#include "utils.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>

#define MAX_BUFFER 16384

struct FILEBUF {
  char buffer[MAX_BUFFER];   // Bufor na dane.
  int timeout;               // Timeout;
  int cur_buf_size, cur_buf_pos;  // Obecny rozmiar/pozycja bufora; 
  int cur_fd;                // Obecny deskryptor;
  struct pollfd poll_fds, poll_out_fds;  // Struktura pollujaca deskryptor.
  pthread_mutex_t mutex;     // Muteks na operacje, w ktorych wazne jest wykluczanie.
};

static bool is_delim(int ch) {
  return (ch == '\r' || ch == '\n' || ch == '\0');
}

// Tworzymy nowy FILEBUF.
FILEBUF *utils_get_fd(int fd) {
  FILEBUF *file = malloc(sizeof(FILEBUF));
  debug("getting addr (%p)\n", file);
  file->cur_fd = fd;
  file->cur_buf_pos = file->cur_buf_size = 0;
  file->timeout = -1;

  file->poll_fds.fd = fd;
  file->poll_fds.events = POLLIN;
  file->poll_out_fds.fd = fd;
  file->poll_out_fds.events = POLLOUT;
  pthread_mutex_init(&file->mutex, NULL);
  return file;
}

// Usuwamy deskryptor z FILEBUF, jesli tylko trafimy.
void utils_erase_fd(FILEBUF *file, int fd) {
  if (file->cur_fd == fd) {
    file->cur_fd = -1;
  }
}

void utils_set_timeout(FILEBUF *file, int timeout) {
  file->timeout = timeout;
}

static int utils_rebuf(FILEBUF *file) {
  if (file->cur_fd == -1) {
    return -1;
  }

  // Pollujemy z timeoutem; jesli nie przejdzie, wypluwamy blad
  int num_waiting = poll(&file->poll_fds, 1, file->timeout);
  if (num_waiting == 0) {
    return -1;
  } else if (num_waiting < 0) {
    return num_waiting;
  }

  if (file->poll_fds.revents & (POLLERR | POLLHUP)) {
    // Zwalilo sie na serwerze, koniec.
    return -1;
  }

  int res = read(file->cur_fd, file->buffer, MAX_BUFFER);
  if (res > 0) {
    file->cur_buf_size = res;
    file->cur_buf_pos  = 0;
  }
  if (res == 0) {
    return FILEBUF_EOF;
  }

  return res;
}

// Pojedynczy znak z serwera.
int utils_readchar(FILEBUF *file) {
  if (file->cur_buf_size == file->cur_buf_pos) {
    int res = utils_rebuf(file);
    if (res == FILEBUF_EOF) {
      return FILEBUF_EOF;
    }
    if (res <= 0) {
      return res - 1;
    }
  }

  return (int)((unsigned char)file->buffer[file->cur_buf_pos++]);
}

// Pojedynczy *telnetowy* znak z serwera.
static int utils_readchar_telnet(FILEBUF *file) {
  int res = utils_readchar(file);
  while (res == 0xFF) {    // 0xFF - sekwencja sterujaca.
    res = utils_readchar(file);
    // 0xFF 0xFF - po prostu bajt 0xFF.
    if (res < 0 || res == 0xFF) { return 0xFF; }

    int ntimes = 1;
    // 0xFF [0xFB, 0xFE] - kolejny bajt.
    if (res >= 0xFB && res <= 0xFE) { ntimes = 2; }

    while (ntimes--) { if ((res = utils_readchar(file)) < 0) { return res; } }
  }

  return res;
}

// getline; rozni sie wersjami; do_telnet ma wiecej separatorow i swojego getchara.
ssize_t utils_getline(FILEBUF *file, char *lineptr, int max_line, bool do_telnet) {
  ssize_t num_read = 0;

  while (num_read < max_line) {
    int ch;
    bool delim;

    if (do_telnet) {
      ch = utils_readchar_telnet(file);
      delim = is_delim(ch);
    } else {
      ch = utils_readchar(file);
      delim = (ch == '\n');
    }

    if (ch < 0) {
      return ch;
    } else if (delim) {
      lineptr[num_read] = 0;
      return num_read;
    } else {
      lineptr[num_read++] = ch;
    }
  }

  return -3;
}

// Blokujace wypisywanie, AZ wypiszemy wszystko.
ssize_t utils_block_write(FILEBUF *file, void *buf, size_t count) {
  if (file->cur_fd < 0) { return -1; }

  ssize_t written = 0;

  pthread_mutex_lock(&file->mutex);

  while (count > 0) {
    int nwait = poll(&file->poll_out_fds, 1, file->timeout);
    if (nwait <= 0 || file->poll_out_fds.revents & (POLLERR | POLLHUP)) {
      pthread_mutex_unlock(&file->mutex);
      return -1;
    }

    ssize_t what = write(file->cur_fd, buf, count);
    if (what <= 0) {
      pthread_mutex_unlock(&file->mutex);
      return -1;
    }
    written += what;
    count -= what;
    buf = (void*)((char*)buf + what);
  }

  pthread_mutex_unlock(&file->mutex);
  
  return written;
}

// Printf blokujacy.
ssize_t utils_block_printf(FILEBUF *file, const char *format, ...) {
  va_list arg;
  char data[1024];

  va_start(arg, format);
  int cnt = vsnprintf(data, 1020, format, arg);
  va_end(arg);

  if (cnt < 0) { return -1; }
  return utils_block_write(file, data, cnt);
}


ssize_t utils_read(FILEBUF *file, void *buf, size_t count) {
  if (file->cur_buf_size == file->cur_buf_pos) {
    int res = utils_rebuf(file);
    if (res <= 0) {
      return res;
    }
  }

  assert(file->cur_buf_pos < file->cur_buf_size);
  int cur_count = file->cur_buf_size - file->cur_buf_pos;
  if (cur_count > (ssize_t)count) {
    cur_count = count;
  }

  memcpy(buf, file->buffer + file->cur_buf_pos, cur_count);
  file->cur_buf_pos += cur_count;
  return cur_count;
}

void utils_free_buf(FILEBUF *file) {
  debug("freeing addr (%p)\n", file);
  free(file);
}



// INNE FUNKCJE


bool is_prefix(const char *text, const char *pattern) {
  int len = strlen(pattern);
  int len_text = strlen(text);

  return (len_text >= len && !strncmp(text, pattern, len));
}


// Parsowanie w tokeny.
char** parse_into_tokens(const char *text) {
  int ntokens = 1;
  char lastch = 0;

  const char *ptr = text, *start;
  while (*ptr) {
    char ch = *ptr++;
    if (ch != ' ' && lastch == ' ') { ntokens++; }
    lastch = ch;
  }

  char** result = (char**)calloc(ntokens + 1, sizeof(char*));
  int cur_token = 0;

  start = ptr = text;
  while (true) {
    if (*ptr == '\0' || *ptr == ' ') {
      if (start != ptr) {
        size_t word_len = ptr - start;
        char *word = (char*)malloc(sizeof(char) * (word_len + 1));
        memcpy(word, start, word_len);
        word[word_len] = 0;
        result[cur_token++] = word;
      }
      start = ptr + 1;
    }

    if (!*ptr) { break; }
    ptr++;
  }

  return result;
}


// Uzyskiwanie tokena.
char* parse_token(char **text, const char *delims) {
  char *res = NULL;
  if (!*text) { return NULL; }

  while (**text) {
    if (strchr(delims, **text)) {
      if (res) {
        **text = 0;
        (*text)++;
        return res;
      }
    } else {
      if (!res) { res = *text; }
    }
    (*text)++;
  }

  return res;
}


int connect_timeout(int fd, const struct sockaddr *addr, socklen_t addrlen) {  
  // http://www.codeproject.com/Tips/168704/How-to-set-a-socket-connection-timeout
  
  unsigned long iMode = 1;
  // Przestawiamy sie w tryb nieblokujacy.
  if (ioctl(fd, FIONBIO, &iMode) < 0) { return -1; }
  
  // Moze uda sie od razu?
  if (connect(fd, addr, addrlen) == 0) { return 0; }
  
  // W tryb blokujacy.
  iMode = 0;
  if (ioctl(fd, FIONBIO, &iMode) < 0) { return -1; }
  
  // Czekamy 5 sekund na akcje.
  struct pollfd fds = {fd, POLLIN | POLLOUT, 0};
  int npolls = poll(&fds, 1, 5 * 1000);
  
  if (npolls <= 0 || (fds.revents & (POLLERR | POLLHUP))) {
    return -1;
  }
  
  return 0;
}
