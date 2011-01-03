/* Generates code for posix_misc.erl
 * more or less just cpp define to epp define convert */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <attr/xattr.h>
#include <fcntl.h>
#include <sys/types.h>


#define D6O(s) D(#s, s, "8#0%.6o") /* define octal */
#define DD(s) D(#s, s, "%d") /* define decimal */
#define D(d, n, f) printf("-define(%s, " f ").\n", d, n);

int main(void) {
    struct utsname uts;
    
    uname(&uts);
    
    printf(
"%% Generated with " __FILE__ " on a system running %s %s\n"
"%% Note that constants may be specific to this OS and version\n"
"\n", uts.sysname, uts.release);
   
    printf("%% stat.st_mode, see man 2 stat for more info\n"); 
    D6O(S_IFMT);
    D6O(S_IFSOCK);
    D6O(S_IFLNK);
    D6O(S_IFREG);
    D6O(S_IFBLK);
    D6O(S_IFDIR);
    D6O(S_IFCHR);
    D6O(S_IFIFO);
    D6O(S_ISUID);
    D6O(S_ISGID);
    D6O(S_ISVTX);
    D6O(S_IRWXU);
    D6O(S_IRUSR);
    D6O(S_IWUSR);
    D6O(S_IXUSR);
    D6O(S_IRWXG);
    D6O(S_IRGRP);
    D6O(S_IWGRP);
    D6O(S_IXGRP);
    D6O(S_IRWXO);
    D6O(S_IROTH);
    D6O(S_IWOTH);
    D6O(S_IXOTH);

    printf("\n");

    printf("%% dirent.d_type\n"); 
    DD(DT_UNKNOWN);
    DD(DT_FIFO);
    DD(DT_CHR);
    DD(DT_DIR);
    DD(DT_BLK);
    DD(DT_REG);
    DD(DT_LNK);
    DD(DT_SOCK);
    DD(DT_WHT);

    printf("\n");
    
    printf("%% See man 2 access\n");
    DD(R_OK);
    DD(W_OK);
    DD(X_OK);
    DD(F_OK);
   
    printf("\n");

    printf("%% See man 2 setxattr\n");
    DD(XATTR_CREATE);
    DD(XATTR_REPLACE);
    
    printf("\n");

    printf("%% See man 2 open\n");
    D6O(O_ACCMODE);
    D6O(O_RDONLY);
    D6O(O_WRONLY);
    D6O(O_RDWR);
    D6O(O_CREAT);
    D6O(O_EXCL);
    D6O(O_NOCTTY);
    D6O(O_TRUNC);
    D6O(O_APPEND);
    D6O(O_NONBLOCK);
    D6O(O_NDELAY);
    D6O(O_SYNC);
    D6O(O_FSYNC);
    D6O(O_ASYNC);

    /*
    D6O(O_DIRECT);
    D6O(O_DIRECTORY);
    D6O(O_NOFOLLOW);
    D6O(O_NOATIME);
    
    D6O(O_DSYNC);
    D6O(O_RSYNC);

    D6O(O_LARGEFILE);
    */

    return 0;
}

