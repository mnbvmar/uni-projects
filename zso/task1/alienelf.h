#ifndef __ALIENELF_H
#define __ALIENELF_H

#define ADDR_MAP_MIN 0x31337000ULL
#define ADDR_MAP_MAX 0x80000000ULL

void process_alien_elf(const char *exec_name, void **arg_loc, int *arg_count);

#endif