TARGET: player master

CC	= cc
CFLAGS	= -Wall -Wuninitialized -Wextra -Wunused -Wshadow -g -O2 -std=gnu11
LFLAGS	= -Wall -pthread


player: player.o err.o headers.o icy.o utils.o fifo_wait.o
	$(CC) $(LFLAGS) $^ -o $@

master: master.o err.o headers.o fifo_wait.o utils.o ssh_conn.o at.o
	$(CC) $(LFLAGS) $^ -o $@ -lssh2 -lrt -lm

.PHONY: clean TARGET
clean:
	rm -f player master *.o *~ *.bak
