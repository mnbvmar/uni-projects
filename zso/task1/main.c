#include "alienelf.h"
#include "defs.h"
#include "trace.h"

#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>


int main(int argc, char **argv) {
  EXPECT_CONDITION(argc >= 2, "Invocation: %s progname [args...]", argv[0]);

  void *cmdargs_loc = NULL;
  int num_args = 0;
  process_alien_elf(argv[1], &cmdargs_loc, &num_args);
  EXPECT_CONDITION(argc == num_args + 2,
                   "Incorrect number of program arguments (got %d, expected %d)",
                   argc - 2, num_args);


  pid_t child_pid = fork();
  EXPECT_CONDITION(child_pid != -1, "Could not fork");

  if (child_pid) {
    parent_runner(child_pid, argv, cmdargs_loc, num_args);
  } else {
    invoke_child(argv[1]);
  }

  __builtin_unreachable();
}
