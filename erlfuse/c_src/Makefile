TOP = ..

SUBDIRS = fuse_forward

TARGET = \
	 $(TOP)/src/posix_errno.erl \
	 $(TOP)/include/posix_misc.hrl

CLEAN = _clean

include $(TOP)/include.mk

posix_errno.h: posix_errno.h.sh
	sh posix_errno.h.sh > $@
posix_errno: posix_errno.erl.c posix_errno.h
	gcc -Wall -o $@ $<
$(TOP)/src/posix_errno.erl: posix_errno
	@# im quite proud of this one :)
	./$< > $@

posix_misc: posix_misc.hrl.c
	gcc -Wall -o $@ $<
$(TOP)/include/posix_misc.hrl: posix_misc
	./$< > $@

_clean:
	rm -f \
	    $(TARGET) \
	    posix_errno \
	    posix_errno.h \
	    posix_misc

