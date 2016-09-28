// Marek Sokolowski - Computer Network large assignment problem
// ICY protocol implementation.
#ifndef __ICY
#define __ICY

#include <stdbool.h>
#include <stdio.h>

#include "utils.h"

#define ICY_MAX_PATH 1024
#define ICY_MAX_METADATA 4096

// Naglowek.
struct icy_conn {
  int server_fd;   // Deskryptor serwera.
  char src_path[ICY_MAX_PATH];   // Sciezka, pod jaka szukac serwera.
  char src_host[ICY_MAX_PATH];   // Nazwa serwera.
  char metadata[ICY_MAX_METADATA];   // Aktualne metadane.
  int metadata_len, to_metadata, metaint;   // Dlugosc, czas do metadanych i odstep.
  bool get_metadata;             // Czy pobierac metadane
  bool started;                  // Czy serwer wystartowal?
  FILEBUF *file;                 // Buforowany deskryptor serwera.
} icy_conn;


// Pobiera headery z serwera. Zwraca true przy sukcesie, false przy porazce.
// Wypisuje na stderr w razie bledu.
bool icy_headers();

// Pobiera dane z serwera. Jesli ponadto !ignore_server, pisze do bufora, lecz
// nie wiecej niz max_data bajtow. Przy okazji parsuje metadane. Zwraca 
// liczbe przeczytanych bajtow.
int icy_getdata(void *buf, int max_data, bool ignore_server);

// Pobiera nazwe aktualnego utworu. Zwraca jego dlugosc.
int icy_gettitle(char *title, int max_title);


#endif
