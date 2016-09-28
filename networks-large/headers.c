// Marek Sokolowski - Computer Network large assignment problem
// Helpful definitions and macros.
#include "headers.h"

int __check_ret_code(int code, const char *cmd) {
  if (code < 0) {
    syserr(cmd);
  }
  return code;
}

int __check_pthread_ret_code(int code, const char *cmd) {
  if (code != 0) {
    syserr(cmd);
  }
  return code;
}
