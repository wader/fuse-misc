#!/bin/sh
#
# autogen.sh 
#
# Requires: automake 1.7, autoconf 2.50
set -e

# Refresh GNU autotools toolchain.
for i in config.guess config.sub missing install-sh mkinstalldirs ; do
	test -r /usr/share/automake-1.7/${i} && {
		rm -f ${i}
		cp /usr/share/automake-1.7/${i} .
	}
	chmod 755 ${i}
done

aclocal-1.7
autoheader
automake-1.7 --foreign --add-missing
autoconf

exit 0