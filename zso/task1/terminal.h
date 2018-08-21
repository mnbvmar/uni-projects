#ifndef __TERMINAL_H
#define __TERMINAL_H

#define TERMINAL_WIDTH 80
#define TERMINAL_HEIGHT 24

void terminal_setup(void);
void terminal_setcursor(int x, int y);
void terminal_putchar(int x, int y, char ch, int color);
int terminal_getchar(void);

#endif