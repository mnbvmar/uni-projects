#define _GNU_SOURCE

#include "defs.h"
#include "syscall.h"
#include "terminal.h"

#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/syscall.h>
#include <sys/uio.h>


#ifndef __NR_getrandom
#define __NR_getrandom 318
#endif


#define EINVAL_MSG(sys_name) ("Invalid argument to syscall " sys_name "()")


static bool check_coords(int x, int y) {
  return 0 <= x && x < TERMINAL_WIDTH &&
         0 <= y && y < TERMINAL_HEIGHT;
}


noreturn void sys_end(pid_t child_pid, int status) {
  kill(child_pid, SIGTERM);
  EXPECT_CONDITION(0 <= status && status <= 63, EINVAL_MSG("end"));
  exit(status);
}

uint32_t sys_getrand(pid_t child_pid) {
  (void)child_pid; // Unused.
  uint32_t result;
  EXPECT_OR_PERROR(syscall(__NR_getrandom, &result, sizeof(uint32_t), 0) != -1,
                   "getrandom() failed");
  return result;
}

int sys_getkey(pid_t child_pid) {
  (void)child_pid; // Unused.
  return terminal_getchar();
}

void sys_print(pid_t child_pid, int x, int y, uint16_t *tracee_chars, int n) {
  EXPECT_CONDITION(check_coords(x, y) && n > 0, EINVAL_MSG("print"));
  
  if (x + n > TERMINAL_WIDTH)
    n = TERMINAL_WIDTH - x;

  const size_t num_bytes = sizeof(uint16_t) * n;

  struct iovec guest_mem_query = {
    .iov_base = tracee_chars,
    .iov_len = num_bytes
  };

  uint16_t *host_chars = (uint16_t *)malloc(num_bytes);
  EXPECT_CONDITION(host_chars != NULL, "Alloc failed");

  struct iovec host_mem_query = {
    .iov_base = host_chars,
    .iov_len = num_bytes
  };

  EXPECT_OR_PERROR(process_vm_readv(child_pid, &host_mem_query, 1, &guest_mem_query, 1, 0) != -1,
                   "Error while reading tracee memory");

  for (int idx = 0; idx < n; ++idx) {
    unsigned char base = host_chars[idx] & 0xFF;
    unsigned char color = (host_chars[idx] >> 8) & 0xF;
    terminal_putchar(x + idx, y, base, color);
  }

  free(host_chars);
}

void sys_setcursor(pid_t child_pid, int x, int y) {
  (void)child_pid; // Unused.
  EXPECT_CONDITION(check_coords(x, y), EINVAL_MSG("setcursor"));
  terminal_setcursor(x, y);
}
