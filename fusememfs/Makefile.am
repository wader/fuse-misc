
EXTRA_DIST = \
autogen.sh

AM_CFLAGS = \
-g \
-Wall \
-D_GNU_SOURCE \
-std=gnu99 \
-D_FILE_OFFSET_BITS=64 \
-D_LARGEFILE_SOURCE

bin_PROGRAMS = fusememfs

fusememfs_CFLAGS = $(AM_CFLAGS) $(GLIB_CFLAGS)
fusememfs_LDADD = $(FUSE_LIBS) $(GLIB_LIBS)
fusememfs_SOURCES = \
fusememfs.c \
namespace.c \
namespace.h

