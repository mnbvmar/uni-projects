KDIR ?= /lib/modules/`uname -r`/build

.PHONY: all default

all: default

default: buffers.c command.c common.c debug.c main.c surface.c
	$(MAKE) -C $(KDIR) M=$$PWD

install:
	$(MAKE) -C $(KDIR) M=$$PWD modules_install

clean:
	$(MAKE) -C $(KDIR) M=$$PWD clean

# main: main.c
# 	gcc main.c -o main
