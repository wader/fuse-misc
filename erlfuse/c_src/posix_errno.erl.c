/* Generates code for posix_errno.erl */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/utsname.h>
#include <errno.h>
#include <attr/xattr.h> /* ENOATTR */

typedef struct error 
{
    char *name;
    int number;
    /* priority, duplicated errnos (same error number),
     * lowest will get the errno -> errno atom mapping */
    int priority;
} error_t;

error_t errors[] = {
#define E(s, p) {#s, s, p}, 
#include "posix_errno.h"
    {0} /* dummy to make syntax correct */
};

int nerrors = sizeof(errors) / sizeof(errors[0]) - 1; /* -1 for {0} */

int cmp_name(const void *a, const void *b) {
    return strcmp(((error_t*)a)->name, ((error_t*)b)->name);
}

int cmp_order(int a, int b) {
    if(a > b)
        return 1;
    else if(a < b)
        return -1;
    else
        return 0;
}

int cmp_number(const void *a, const void *b) {
    int n = cmp_order(((error_t*)a)->number, ((error_t*)b)->number);

    if(n == 0)
        return cmp_order(((error_t*)a)->priority, ((error_t*)b)->priority);
    else
        return n;
}

char *strtolower(char *s) {
    char *p;
    s = p = strdup(s);

    while(*p) {
        *p = tolower(*p);
        p++;
    }
    
    return s;
}

int main(void) {
    int i;
    struct utsname uts;
    
    uname(&uts);
    
    printf(
"%% Generated with " __FILE__ " on a system running %s %s\n"
"%% Note that constants may be specific to this OS and version\n"
"\n"
"-module(posix_errno).\n"
"-export([atom_to_errno/1,\n"
"         errno_to_atom/1,\n"
"         string/1])."
"\n"
"\n", uts.sysname, uts.release);
    
    /* atom_to_errno/1 */
    qsort(errors, nerrors, sizeof(errors[0]), cmp_name);
    for(i = 0; i < nerrors; i++) {
        printf("atom_to_errno(%s) -> %d;\n",
               strtolower(errors[i].name), errors[i].number);
    }
    printf("atom_to_errno(X) ->\n"
           "    erlang:error({unknown_posix_errno_atom, X}).\n");
    printf("\n");

    /* errno_to_atom/1 */
    qsort(errors, nerrors, sizeof(errors[0]), cmp_number);
    for(i = 0; i < nerrors; i++) {
        int dup = 0;
            
        /* take care of duplicates, eg EAGAIN and EWOULDBLOCK has same errno */
        if(i > 0 && errors[i-1].number == errors[i].number) {
            if(!dup)
                printf(" %%");
            printf(" %s", strtolower(errors[i].name));
            dup = 1;
        } else  {
            if(dup || i != 0)
                printf("\n");
            printf("errno_to_atom(%d) -> %s;",
                   errors[i].number, strtolower(errors[i].name));
            dup = 0;
        }
    }
    printf("\n");
    printf("errno_to_atom(X) ->\n"
           "    erlang:error({unknown_posix_errno, X}).\n");
    printf("\n");
    
    /* string/1 */
    qsort(errors, nerrors, sizeof(errors[0]), cmp_name);
    for(i = 0; i < nerrors; i++) {
        printf("string(%s) -> \"%s\";\n",
               strtolower(errors[i].name), strerror(errors[i].number));
    }
    printf("string(X) when is_integer(X) ->\n");
    printf("    string(errno_to_atom(X));\n");
    printf("string(X) ->\n"
           "    erlang:error({unknown_posix_errno_atom, X}).\n");

    return 0;
}

