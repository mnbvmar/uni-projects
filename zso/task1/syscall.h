#ifndef __SYSCALL_H
#define __SYSCALL_H

#include <stdint.h>
#include <stdnoreturn.h>

#include <sys/types.h>

noreturn void sys_end(pid_t child_pid, int status);
uint32_t sys_getrand(pid_t child_pid);
int sys_getkey(pid_t child_pid);
void sys_print(pid_t child_pid, int x, int y, uint16_t *tracee_chars, int n);
void sys_setcursor(pid_t child_pid, int x, int y);

#endif