CFLAGS = -Wall -Wextra -Werror -std=gnu11
LDFLAGS = -lncurses
SRC = $(wildcard *.c)
OBJS = $(patsubst %.c, %.o, $(SRC))

.PHONY: all clean

all: emu

clean:
	rm -f *.o emu

emu: $(OBJS)
	$(CC) -o emu $(OBJS) $(CFLAGS) $(LDFLAGS)

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) $<


#tracer: trace.c terminal.c
#	gcc -o tracer alienelf.c main.c syscall.c trace.c terminal.c -no-pie -Wall -Wshadow -lncurses
