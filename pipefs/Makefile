
CFLAGS = $(shell pkg-config --cflags fuse) -Wall -ggdb -DFUSE_USE_VERSION=26
LDFLAGS = $(shell pkg-config --libs fuse)

all: pipefs

clean:
	rm pipefs pipefs.o

pipefs: pipefs.o

