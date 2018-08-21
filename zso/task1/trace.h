#ifndef __TRACE_H
#define __TRACE_H

#include <stdnoreturn.h>

noreturn void invoke_child(const char *exec_name);
noreturn void parent_runner(pid_t child_pid, char **argv, void *cmdargs_loc, int num_args);

#endif