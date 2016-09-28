// Marek Sokolowski - Computer Network large assignment problem
// ICY protocol implementation.
#include "icy.h"
#include "utils.h"
#include "headers.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_TMP_BUF 8192
#define SERVER_WAIT_TIME 5000

static char data_buf[MAX_TMP_BUF];


static const char* icy_header_pattern =
  "GET %s HTTP/1.0\r\n"
  "Host: %s\r\n"
  "Accept: */*\r\n"
  "%s"
  "Connection: close\r\n"
  "\r\n";


bool icy_headers() {
  // Tworzymy deskryptor, ustawiamy timeout 5s i wklejamy naglowki.
  icy_conn.file = utils_get_fd(icy_conn.server_fd);
  utils_set_timeout(icy_conn.file, SERVER_WAIT_TIME);

  int len = snprintf(data_buf, MAX_TMP_BUF, icy_header_pattern,
                               icy_conn.src_path,
                               icy_conn.src_host,
                               icy_conn.get_metadata ? "Icy-MetaData: 1\r\n" : "");
  utils_block_write(icy_conn.file, data_buf, len);

#define MAX_HEADER_LINE 1024
  char line[MAX_HEADER_LINE];

  ssize_t linesize;

  // Czytamy kolejne linie z bufora.
  while ((linesize = utils_getline(icy_conn.file, line, MAX_HEADER_LINE, false))) {
    if (linesize <= 0) {
      // Koniec lub blad serwera.
      return false;
    }

    if (linesize == 1) { break; }

    // Czy informacja o metaincie?
    if (is_prefix(line, "icy-metaint:")) {
      char *pos = line;
      while (*pos && *pos != ':') { pos++; }  // Przesuwamy się za dwukropek.
      pos++;
      while (*pos && isspace(*pos)) { pos++; } // Przesuwamy wszystkie whitespaces.
      char *bad_pos;

      long meta_len = strtol(pos, &bad_pos, 10);
      if (!*pos || *bad_pos != '\r' || meta_len <= 0 || meta_len > 1e9) {
        // Zla liczba!
        return false;
      }

      icy_conn.get_metadata = true;
      icy_conn.metaint = meta_len;
      icy_conn.to_metadata = meta_len;
    }
  }

  // Jesli chcielismy metadanych, a ich nie dostaniemy, to blad.
  if (icy_conn.get_metadata && icy_conn.metaint == 0) { return false; }

  icy_conn.started = true;

  return true;
#undef MAX_HEADER_LINE
}


int icy_getdata(void *buf, int max_data, bool ignore_server) {
  int nread = 0;
  bool get_meta = icy_conn.get_metadata;

  while (nread < max_data) {
    // Pobieramy dane znak po znaku.
    int ch = utils_readchar(icy_conn.file);
    if (ch == -1) {
      return nread;
    } else if (ch == -2) {
      return -1;
    }

    // Zaczynamy metadane.
    if (get_meta && icy_conn.to_metadata == 0) {
      // Czytamy --calosc-- metadanych.
      int meta_len = ch * 16;
      if (meta_len == 0) {
        debug("No metadata for now\n");
        icy_conn.to_metadata = icy_conn.metaint;
        continue;
      }

      if (ignore_server) {
        debug("Ignoring metadata of length %d\n", meta_len);
        for (int i = 0; i < meta_len; i++) {
          int x = utils_readchar(icy_conn.file);
          if (x == -1) { return nread; }
          if (x == -2) { return -1; }
        }
      } else {
        debug("Getting metadata, length = %d\n", meta_len);
        for (int i = 0; i < meta_len; i++) {
          int x;
          icy_conn.metadata_len = i;
          icy_conn.metadata[i] = x = utils_readchar(icy_conn.file);
          if (x == -1) { return nread; }
          if (x == -2) { return -1; }
        }

        icy_conn.metadata_len = meta_len;
        icy_conn.metadata[icy_conn.metadata_len] = 0;
      }

      icy_conn.to_metadata  = icy_conn.metaint;
    } else {
      // Jesli to nie sa metadane, przepisujemy do bufora.
      char *cbuf = buf;
      *cbuf = ch;
      cbuf++;
      nread++;
      buf = cbuf;
      icy_conn.to_metadata--;
    }

  }

  return nread;
}


int icy_gettitle(char *title, int max_title) {
  char *ch = strstr(icy_conn.metadata, "StreamTitle='");
  if (ch == NULL) { return 0; }
  while (*ch != '\'') { ch++; }
  ch++;

  char *chend = strstr(ch, "\';");
  if (chend == NULL) { return 0; }

  int title_len = 0;
  while (title_len < max_title && ch != chend) {
    title[title_len++] = *ch++;
  }

  return title_len;
}
