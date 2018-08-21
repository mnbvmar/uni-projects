#define _GNU_SOURCE

#include "alienos.h"
#include "defs.h"
#include "syscall.h"
#include "terminal.h"
#include "trace.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/user.h>
#include <sys/wait.h>

#include <linux/ptrace.h>


static inline bool is_int(long value) {
  return INT_MIN <= value && value <= INT_MAX;
}

static void set_args(pid_t child_pid, char **input_arg_loc, void *arg_loc, int arg_count) {
  int *args = (int *)malloc(sizeof(int) * arg_count);
  for (int idx = 0; idx < arg_count; ++idx) {
    char *endptr;
    long int arg_value = strtol(input_arg_loc[idx], &endptr, 10);
    EXPECT_CONDITION(*input_arg_loc[idx] != '\0' &&
                     *endptr == '\0' &&
                     is_int(arg_value),
                     "Invalid numeric argument: \"%s\"", input_arg_loc[idx]);
    args[idx] = (int)arg_value;
  }

  const size_t num_bytes = sizeof(int) * arg_count;

  struct iovec guest_mem_query = {
    .iov_base = arg_loc,
    .iov_len = num_bytes
  };

  struct iovec host_mem_query = {
    .iov_base = args,
    .iov_len = num_bytes
  };

  EXPECT_NEQ(process_vm_writev(child_pid, &host_mem_query, 1, &guest_mem_query, 1, 0), -1,
            "Could not store the command line arguments in tracee's memory");
  free(args);
}

noreturn void invoke_child(const char *exec_name) {
  EXPECT_OR_PERROR(prctl(PR_SET_PDEATHSIG, SIGKILL, 0, 0, 0) == 0,
                   "prctl failed on tracee");
  EXPECT_OR_PERROR(ptrace(PTRACE_TRACEME, 0, NULL, NULL) != -1,
                   "Could not invoke ptrace on child");
  EXPECT_OR_PERROR(execl(exec_name, exec_name, NULL) != -1,
                   "Error loading alien's executable file");
  __builtin_unreachable();
}

noreturn void parent_runner(pid_t child_pid, char **argv, void *arg_loc, int num_args) {
  terminal_setup();

  int wstatus;
  EXPECT_OR_PERROR(waitpid(child_pid, &wstatus, 0) == child_pid,
                   "Lost the tracee");
  EXPECT_OR_PERROR(ptrace(PTRACE_SETOPTIONS, child_pid, NULL, PTRACE_O_TRACESYSGOOD) != -1,
                   "Could not set up the tracee");
  set_args(child_pid, argv + 2, arg_loc, num_args);

  while (1) {
    EXPECT_OR_PERROR(ptrace(PTRACE_SYSEMU, child_pid, NULL, NULL) != -1,
                     "Could not emulate the tracee");
    EXPECT_OR_PERROR(waitpid(child_pid, &wstatus, 0) == child_pid,
                     "Lost the tracee");
    EXPECT_CONDITION(WIFSTOPPED(wstatus), "Tracee didn't stop when emulated");

    if (WSTOPSIG(wstatus) == (0x80 | SIGTRAP)) {
      // read registers
      struct user_regs_struct reg_info;
      EXPECT_OR_PERROR(ptrace(PTRACE_GETREGS, child_pid, NULL, &reg_info) != -1,
                       "Could not fetch tracee registers");

      switch (reg_info.orig_rax) {
        case 0:
          sys_end(child_pid, reg_info.rdi);
          break;

        case 1:
          reg_info.rax = sys_getrand(child_pid);
          break;

        case 2:
          reg_info.rax = sys_getkey(child_pid);
          break;

        case 3:
          sys_print(child_pid,
                    reg_info.rdi,
                    reg_info.rsi,
                    (uint16_t *)reg_info.rdx,
                    reg_info.r10);
          break;

        case 4:
          sys_setcursor(child_pid, reg_info.rdi, reg_info.rsi);
          break;

        default:
          EXPECT_CONDITION(false, "Invalid syscall index");
      }

      EXPECT_OR_PERROR(ptrace(PTRACE_SETREGS, child_pid, NULL, &reg_info) != -1,
                       "Could not update tracee registers");
    } else {
      const int prog_signal = WSTOPSIG(wstatus);
      EXPECT_CONDITION(prog_signal == 0,
                       "Process caught an unexpected signal %d",
                       prog_signal);
    }
  }
}
