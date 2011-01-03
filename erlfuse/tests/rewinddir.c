#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include <dirent.h>

void readdirn(DIR *d, int n) {
    struct dirent *de;
    int i;
    int doff = 0;

    for(i = 0; i < n; i++) {
        de = readdir(d);
        if(de == NULL)
            break;

        printf("ino=%d off=%d (%d) reclen=%d name=%s type=%d\n",
               de->d_ino, de->d_off, de->d_off - doff, de->d_reclen,
               de->d_name, de->d_type);

        doff = de->d_off;
    }
}

int main(int argc, char **argv) {
    DIR *d;

    d = opendir(argv[1]);
    
    readdirn(d, atoi(argv[2]));
    printf("rewind\n");
    rewinddir(d);
    readdirn(d, atoi(argv[3]));
    
    void rewinddir(DIR *dir);

    return 0;
}

