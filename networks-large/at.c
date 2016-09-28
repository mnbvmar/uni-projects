// Marek Sokolowski - Computer Networks large assignment.
// Library setting up time-dependent actions.
#include "at.h"
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

static const int digits[4] = {0, 1, 3, 4};   // Pozycje cyfr.

long parse_time(const char *str) {
  // Sprawdzamy poprawnosc inputu.
  if (strlen(str) != 5) { return -1; }
  if (str[2] != '.') { return -1; }
  for (int i = 0; i < 4; i++) {
    if (!isdigit(str[digits[i]])) { return -1; }
  }
  
  int hours   = strtol(str, NULL, 10),
      minutes = strtol(str + 3, NULL, 10);
  
  // Liczymy na strukturach systemowych timestamp momentu w czasie tego samego
  // dnia i o podanej godzinie.
  time_t cur_raw_time, fire_raw_time;
  struct tm *cur_time, fire_time;

  time(&cur_raw_time);
  cur_time = localtime(&cur_raw_time);

  memcpy(&fire_time, cur_time, sizeof(struct tm));
  fire_time.tm_hour = hours;
  fire_time.tm_min = minutes;
  fire_time.tm_sec = 0;
  fire_raw_time = mktime(&fire_time);
  
  // Liczymy roznice. Jesli wyszla ujemna, musimy dodac 1 dzien.
  double dist = difftime(fire_raw_time, cur_raw_time);
  if (dist < 1e-7) {
    dist += 24 * 60 * 60;
  }
  
  return (int)round(dist);
}


bool do_in_time(long seconds, void (*action)(union sigval), int value) {
  // Korzystamy z timerow systemowych.
  struct sigevent sevp;
  memset(&sevp, 0, sizeof(struct sigevent));
  sevp.sigev_value.sival_int = value;
  sevp.sigev_notify = SIGEV_THREAD;
  sevp.sigev_notify_function = action;
  
  timer_t tid;
  int err = timer_create(CLOCK_REALTIME, &sevp, &tid);
  if (err) {
    return false;
  }
  
  struct itimerspec timer_val;
  memset(&timer_val, 0, sizeof(timer_val));
  timer_val.it_value.tv_sec = seconds;
  timer_settime(tid, 0, &timer_val, NULL);
  return true;
}
