// Marek Sokolowski - Computer Network large assignment problem
// Helpful definitions and macros.
#ifndef __HEADERS
#define __HEADERS

#include "err.h"

int __check_ret_code(int code, const char *cmd);

// Makro uruchamiajace funkcje cmd i sprawdzajace kod wyjscia. Jesli jest ujemny
// (jak w wiekszosci funkcji systemowych przy bledzie), wywala sie z odpowiednim
// komunikatem bledu i kodem bledu 1.
#define RUN_CMD(cmd, ...) __check_ret_code(cmd(__VA_ARGS__), #cmd)


int __check_pthread_ret_code(int code, const char *cmd);

// Podobnie, jak wyzej, lecz tutaj za blad uznawany jest niezerowy return z
// funkcji.
#define RUN_THREAD_CMD(cmd, ...) __check_pthread_ret_code(cmd(__VA_ARGS__), #cmd)

#define MAX_PORT 65535


// Makro sluzace do debugowania. W przypadku braku flagi -DDEBUG nic nie robi.
#ifdef DEBUG
#define debug(...) fprintf(stderr, __VA_ARGS__)
#else
#define debug(...) do {} while(0)
#endif


// Zmienna srodowiskowa, ktora, gdy ustawiona, zmienia zachowanie playera.
// Zaklada on, ze jest uruchamiany przez SSH, wiec nie pozwala na wypisywanie
// do pliku; ponadto, co jakis czas wysyla serwerowi keepalive.
#define REMOTE_RUN "PLAYER_REMOTE_RUN"


#endif
