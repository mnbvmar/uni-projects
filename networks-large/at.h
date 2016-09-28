// Marek Sokolowski - Computer Networks large assignment.
// Library setting up time-dependent actions.
#ifndef __AT
#define __AT
#include <signal.h>
#include <stdbool.h>
#include <time.h>

// Wczytaj date postaci XX.YY i zmien na liczbe sekund do najblizszego wystapienia
// godziny XX:YY:00. (W przypadku bledu zwraca -1).
long parse_time(const char *str);

// Za 'seconds' sekund wywolaj funkcje 'action' z parametrem 'arg'.
bool do_in_time(long seconds, void (*action)(union sigval), int arg);

#endif
