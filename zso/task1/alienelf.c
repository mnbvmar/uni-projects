#include "alienelf.h"
#include "alienos.h"
#include "defs.h"

#include <elf.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static inline bool is_header_type_ok(int p_type) {
  switch (p_type) {
    case PT_LOAD:
    case PT_NULL:
    case PT_PARAMS:
      return true;
    
    default:
      return false;
  }
}

static const char *err_elf = "Bad alien x86-64 ELF format";


void process_alien_elf(const char *exec_name, void **arg_loc, int *arg_count) {
  *arg_loc = NULL;
  *arg_count = 0;

  FILE *fp = fopen(exec_name, "rb");
  EXPECT_OR_PERROR(fp != NULL, "Could not open the executable for reading");

  Elf64_Ehdr elf_hdr;
  EXPECT_EQ(fread(&elf_hdr, sizeof(elf_hdr), 1, fp), 1, err_elf);
  EXPECT_EQ(memcmp(&elf_hdr.e_ident[EI_MAG0], ELFMAG, SELFMAG), 0, err_elf);
  EXPECT_EQ(elf_hdr.e_ident[EI_CLASS], ELFCLASS64, err_elf); // 64-bit
  EXPECT_EQ(elf_hdr.e_ident[EI_DATA], ELFDATA2LSB, err_elf); // little-endian
  EXPECT_EQ(elf_hdr.e_ident[EI_VERSION], EV_CURRENT, err_elf); // ELF v1
  EXPECT_EQ(elf_hdr.e_type, ET_EXEC, err_elf); // executable
  EXPECT_EQ(elf_hdr.e_machine, EM_X86_64, err_elf); // x86-64
  EXPECT_EQ(elf_hdr.e_version, EV_CURRENT, err_elf); // ELF v1

  void *exe_entry = (void *)elf_hdr.e_entry;
  EXPECT_CONDITION((void *)ADDR_MAP_MIN <= exe_entry &&
                   exe_entry <= (void *)ADDR_MAP_MAX,
                   err_elf);

  const uint64_t program_header_start = elf_hdr.e_phoff;
  EXPECT_EQ(elf_hdr.e_ehsize, sizeof(elf_hdr), err_elf);
  const uint16_t program_header_entry_size = elf_hdr.e_phentsize;
  int program_header_num_entries = elf_hdr.e_phnum;

  EXPECT_CONDITION(fseek(fp, 0L, SEEK_END) == 0, err_elf);
  const uint64_t file_size = ftell(fp);
  
  EXPECT_CONDITION(program_header_start + (uint64_t)program_header_num_entries *
                                          program_header_entry_size <=
                                          file_size,
                   err_elf);

  EXPECT_CONDITION(fseek(fp, program_header_start, SEEK_SET) == 0, err_elf);

  *arg_loc = NULL;
  *arg_count = 0;

  const size_t alloc_size = sizeof(uint64_t) * program_header_num_entries;
  uint64_t *vmem_start = (uint64_t *)malloc(alloc_size);
  uint64_t *vmem_size = (uint64_t *)malloc(alloc_size);
  uint64_t *vmem_end = (uint64_t *)malloc(alloc_size);
  bool *is_pt_load = (bool *)malloc(sizeof(bool) * program_header_num_entries);
  EXPECT_CONDITION(vmem_start && vmem_size && vmem_end && is_pt_load,
                   "Memory failure in process_alien_elf()");

  for (int idx = 0; idx < program_header_num_entries; ++idx) {
    Elf64_Phdr program_hdr;
    EXPECT_EQ(fread(&program_hdr, sizeof(program_hdr), 1, fp), 1, err_elf);

    EXPECT_CONDITION(is_header_type_ok(program_hdr.p_type), err_elf);

    // These headers have no impact on the virtual memory layout.
    if (program_hdr.p_type == PT_NULL || program_hdr.p_memsz == 0)
      continue;

    // Virtual memory location should be correct.
    vmem_start[idx] = program_hdr.p_vaddr;
    vmem_size[idx] = program_hdr.p_memsz;
    vmem_end[idx] = vmem_start[idx] + vmem_size[idx];
    is_pt_load[idx] = program_hdr.p_type == PT_LOAD;

    // Check for overflow.
    EXPECT_CONDITION(vmem_end[idx] >= vmem_start[idx], err_elf);

    // Check if the segment is in the appropriate location.
    EXPECT_CONDITION(ADDR_MAP_MIN <= vmem_start[idx], err_elf);
    EXPECT_CONDITION(vmem_end[idx] <= ADDR_MAP_MAX, err_elf);

    // Additional checks if the segment is of type PT_PARAMS.
    if (program_hdr.p_type == PT_PARAMS) {
      EXPECT_EQ(*arg_loc, NULL, err_elf);
      EXPECT_EQ(vmem_size[idx] % 4, 0, err_elf);
      *arg_loc = (void *)vmem_start[idx];
      *arg_count = (int)(vmem_size[idx] / 4);
    }
  }

  fclose(fp);

  // Check if the PT_PARAMS section (if there is any) is strictly contained
  //   in a single PT_LOAD segment.
  if (*arg_loc != NULL) {
    const void *args_start = *arg_loc;
    const void *args_end = args_start + sizeof(int) * *arg_count;
    bool is_inside_pt_load = false;
    for (int idx = 0; idx < program_header_num_entries; ++idx) {
      if (is_pt_load[idx] &&
          (void *)vmem_start[idx] <= args_start &&
          args_end <= (void *)vmem_end[idx]) {
        is_inside_pt_load = true;
        break;
      }
    }

    EXPECT_CONDITION(is_inside_pt_load, err_elf);
  }

  free(vmem_start);
  free(vmem_size);
  free(vmem_end);
  free(is_pt_load);
}