#!/bin/sh
# Generates code for posix_errno.h, contains macro calls for all error defines
# in errno.h

echo -e "#include <errno.h>\n#include <attr/xattr.h>" | \
    cpp -dD | \
    grep "^#define E" | \
    sed -e '
/\(ENOATTR\|EWOULDBLOCK\)/ {
    s/#define \(.*\) .*/E(\1, 1)/g
}
{
    s/#define \(.*\) .*/E(\1, 0)/g
}
'

