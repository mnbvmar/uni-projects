#ifndef __DEFS_H
#define __DEFS_H

#include <curses.h>
#include <stdio.h>
#include <stdlib.h>

#define ERR_UNRECOVERABLE 127

#define EXPECT_CONDITION(cond, err_msg, ...) do {   \
    if (!(cond)) {                                  \
      endwin();                                     \
      fprintf(stderr, "Error: ");                   \
      fprintf(stderr, err_msg, ##__VA_ARGS__);      \
      fprintf(stderr, "\n");                        \
      exit(ERR_UNRECOVERABLE);                      \
    }                                               \
  } while(0)

#define EXPECT_OR_PERROR(cond, err_prefix) do {   \
    if (!(cond)) {                                \
      endwin();                                   \
      perror(err_prefix);                         \
      exit(ERR_UNRECOVERABLE);                    \
    }                                             \
  } while(0)

#define EXPECT_EQ(lhs, rhs, err_msg, ...) \
  EXPECT_CONDITION((lhs) == (rhs), err_msg, ##__VA_ARGS__)

#define EXPECT_NEQ(lhs, rhs, err_msg, ...) \
  EXPECT_CONDITION((lhs) != (rhs), err_msg, ##__VA_ARGS__)


#endif