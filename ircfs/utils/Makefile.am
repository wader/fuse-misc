
EXTRA_DIST = \
irc2numerics_generate

AM_CFLAGS = -Wall

noinst_PROGRAMS = irctest

irctest_CFLAGS = $(AM_CFLAGS) $(GLIB_CFLAGS) -DIRCTEST -I../
irctest_LDADD = $(GLIB_LIBS)
irctest_SOURCES = \
irctest.c \
../irc.c \
../irc.h

