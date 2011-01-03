TOP = ../..

ERL_INTERFACE=$(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(erl_interface)])' -s erlang halt)

CFLAGS = \
	$(shell pkg-config --cflags fuse) \
	-I$(ERL_INTERFACE)/include \
	-Wall -ggdb \
	-DFUSE_USE_VERSION=26

LDFLAGS = \
	$(shell pkg-config --libs fuse) \
	-L$(ERL_INTERFACE)/lib \
	-lerl_interface -lei

BIN = fuse_forward
OBJS = fuse_forward.o

include $(TOP)/include.mk


