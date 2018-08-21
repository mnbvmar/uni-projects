#include "alienos.h"
#include "defs.h"
#include "terminal.h"

#include <curses.h>

static int screeny, screenx;

static const int default_colors[8] = {
  COLOR_BLACK,
  COLOR_BLUE,
  COLOR_GREEN,
  COLOR_CYAN,
  COLOR_RED,
  COLOR_MAGENTA,
  COLOR_YELLOW,
  COLOR_WHITE
};

static void terminal_teardown(void) {
  endwin();
}

void terminal_setup(void) {
  static const char *init_fail_msg =
    "Could not set up terminal (maybe it doesn't support colors?)";

  initscr();  // terminates on failure
  atexit(terminal_teardown);
  EXPECT_CONDITION(has_colors(), init_fail_msg);
  EXPECT_CONDITION(start_color() == OK, init_fail_msg);
  EXPECT_CONDITION(cbreak() == OK, init_fail_msg);
  EXPECT_CONDITION(noecho() == OK, init_fail_msg);
  EXPECT_CONDITION(keypad(stdscr, TRUE) == OK, init_fail_msg);
  screenx = screeny = 0;
  for (int i = 0; i < 8; ++i) {
    EXPECT_CONDITION(init_pair(i + 16, default_colors[i], COLOR_BLACK) == OK, init_fail_msg);
  }
}

static const char *term_fail_msg = "A terminal operation failed";

void terminal_setcursor(int x, int y) {
  screenx = x;
  screeny = y;
  EXPECT_CONDITION(move(y, x) == OK, term_fail_msg);
  EXPECT_CONDITION(wrefresh(stdscr) == OK, term_fail_msg);
}

void terminal_putchar(int x, int y, char ch, int color) {
  int color_low = color & 7;

  int color_pair = COLOR_PAIR(color_low + 16);
  if (color < 8)
    color_pair |= A_DIM;
  else
    color_pair |= A_BOLD;

  EXPECT_CONDITION(move(y, x) == OK, term_fail_msg);
  // Can't check for ERR here; addch spits out ERR when the cursor gets
  // out of the screen. This happens when our terminal is exactly
  // 24 rows high and we write anything at its lower right corner.
  addch(ch | color_pair);
  EXPECT_CONDITION(move(screeny, screenx) == OK, term_fail_msg);
}

int terminal_getchar(void) {
  int ch = getch();
  EXPECT_CONDITION(ch != ERR, term_fail_msg);

  switch (ch) {
    case KEY_UP: return ALIENOS_KEY_UP;
    case KEY_LEFT: return ALIENOS_KEY_LEFT;
    case KEY_DOWN: return ALIENOS_KEY_DOWN;
    case KEY_RIGHT: return ALIENOS_KEY_RIGHT;
    case KEY_ENTER: return 0x0A;
    default: return ch;
  }
}
