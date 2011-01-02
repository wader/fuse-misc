
SUBDIRS = utils

EXTRA_DIST = \
autogen.sh \
AUTHORS \
HACKING \
NOTES

AM_CFLAGS = \
-g \
-Wall \
-D_GNU_SOURCE \
-std=gnu99 \
-D_FILE_OFFSET_BITS=64 \
-D_LARGEFILE_SOURCE

bin_PROGRAMS = ircfs

ircfs_CFLAGS = $(AM_CFLAGS) $(GLIB_CFLAGS)
ircfs_LDADD = $(FUSE_LIBS) $(GLIB_LIBS)
ircfs_SOURCES = \
ircfs.c \
ircfs.h \
irc.c \
irc.h \
irc_data.c \
irc_data.h \
irc_callbacks.c \
irc_callbacks.h \
fuse_callbacks.c \
fuse_callbacks.h

