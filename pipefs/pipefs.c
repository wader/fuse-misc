/*
 * pipefs, program output as file content
 * mattias.wadman@gmail.com
 *
 * config file:
 *
 * bla: echo "bla"
 * tvchannel.ts: dvbcat -s dvbd.socket dvt-t tvchannel
 *
 * TODO:
 * short or zero byte count from read does not seam to
 * make programs stop reading.. hmm
 *
 */

#define _GNU_SOURCE /* strndup */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <libgen.h>
#include <stddef.h>
#include <time.h>
#include <assert.h>

#define FUSE_USE_VERSION 26
#include <fuse.h>


struct pipefs_opts {
    char *config;
};

typedef struct pipe_s {
    char *name;
    char *command;
} pipe_t;

pipe_t **pipes;
int pipes_count;
struct pipefs_opts opts;

static void pipefs_read_config(void) {
    FILE *f;
    char b[1024];

    f = fopen(opts.config, "r");
    if(f == NULL)
        return;

    pipes = NULL;
    pipes_count = 0;
    while(fgets(b, sizeof(b), f) != NULL) {
        char *s;
        int l;
        
        s = strchr(b, ':');
        if(s == NULL)
            continue;

        pipes = realloc(pipes, sizeof(pipes) * (pipes_count + 1));
        pipes[pipes_count] = malloc(sizeof(**pipes));

        pipes[pipes_count]->name = strndup(b, s - b);
       
        s++;  /* skip : */
        /* skip leading spaces */
        while(*s == ' ')
            s++;
        /* remove ending \n */
        l = strlen(s);
        if(s[l-1] == '\n')
            s[l-1] = '\0';
        pipes[pipes_count]->command = strdup(s);
        
        pipes_count++;
    }

    fclose(f);
}

static pipe_t *pipefs_find(const char *name) {
    int i;

    for(i = 0; i < pipes_count; i++) {
        if(strcmp(name, pipes[i]->name) == 0)
            return pipes[i];
    }

    return NULL;
}


static int pipefs_access(const char *path, int mask)
{
    if(strcmp(path, "/") == 0)
        return 0;

    if(!pipefs_find(path + 1))
        return -ENOENT;

    return 0;
}

static int pipefs_getattr(const char *path, struct stat *stbuf)
{
    pipe_t *p;
    
    if(strcmp(path, "/") == 0) {
        stbuf->st_mode = S_IFDIR | 0755;
        stbuf->st_nlink = 3; /* . + .. + fake node */

        return 0;
    }

    p = pipefs_find(path + 1);
    if(!p)
        return -ENOENT;

    memset(stbuf, 0, sizeof(struct stat));
    stbuf->st_uid = 0;
    stbuf->st_gid = 0;
    stbuf->st_mode = S_IFREG | 0644;
    stbuf->st_nlink = 1; 
    stbuf->st_size = ~0;
    stbuf->st_atime = 
        stbuf->st_mtime = 
        stbuf->st_ctime = 0;
	
    return 0;
}


static int pipefs_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                       off_t offset, struct fuse_file_info *fi)
{
    int i;

    for(i = 0; i < pipes_count; i++) {
        struct stat st;

        memset(&st, 0, sizeof(st));

        if (filler(buf, pipes[i]->name, &st, 0))
            break;
    }

    return 0;
}

typedef struct pipe_process_s
{
    FILE *stream;
} pipe_process_t;

static int pipefs_open(const char *path, struct fuse_file_info *fi)
{
    pipe_t *p;
    pipe_process_t *pp;

    p = pipefs_find(path + 1);
    if(!p)
        return -ENOENT;

    pp = malloc(sizeof(pipe_process_t));

    pp->stream = popen(p->command, "r");
    if(!pp->stream) {
        free(pp);

        return -EINVAL;
    }
    
    fi->fh = (uint64_t)(uint32_t)pp;
    
    return 0;
}

static int pipefs_read(const char *path, char *buf, size_t size, off_t offset,
                    struct fuse_file_info *fi)
{
    pipe_process_t *pp = (pipe_process_t *)(uint32_t)fi->fh;

    return fread(buf, 1, size, pp->stream);
}

static int pipefs_release(const char *path, struct fuse_file_info *fi)
{
    pipe_process_t *pp = (pipe_process_t *)(uint32_t)fi->fh;

    pclose(pp->stream);

    free(pp);
    
    return 0;
}

static int pipefs_statfs(const char *path, struct statvfs *stbuf)
{
    return 0;
}

static struct fuse_operations pipefs_oper = {
    .getattr	= pipefs_getattr,
    .access	= pipefs_access,
    .readdir	= pipefs_readdir,
    .open	= pipefs_open,
    .read	= pipefs_read,
    .statfs	= pipefs_statfs,
    .release	= pipefs_release
};

enum {
    KEY_HELP
};

static struct fuse_opt pipefs_opts[] = {
       {"config=%s", offsetof(struct pipefs_opts, config), 0},
       FUSE_OPT_KEY("-h", KEY_HELP),
       FUSE_OPT_KEY("--help", KEY_HELP),
       FUSE_OPT_END
};

static int pipefs_opt_proc(void *data, const char *arg, int key,
                           struct fuse_args *outargs)
{
    struct pipefs_opts *opts = (struct pipefs_opts *)data;

    switch (key) {
    case FUSE_OPT_KEY_OPT:
        /* pass on, keep */
        return 1;
        break;
    case FUSE_OPT_KEY_NONOPT:
        /* mountpoint, keep */
        return 1;
        break;
    case KEY_HELP:
        fprintf(stderr,
                "Usage: %s mountpoint [options]\n"
                "\n"
                "pipefs options:\n"
                "    -o config=FILE         config file (%s)\n"
                "\n"
                "",
                outargs->argv[0],
                opts->config
                );
        /* help with no header */
        fuse_opt_add_arg(outargs, "-ho");
        /* run main just to make it show help */
        fuse_main(outargs->argc, outargs->argv, &pipefs_oper, NULL);
        exit(1);
        break;
    default:
        printf("invalid arg?\n");
        exit(1);
        break;
    }
}

int main(int argc, char *argv[])
{
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);

    opts.config = "pipefs.conf";

    if(fuse_opt_parse(&args, &opts, pipefs_opts, pipefs_opt_proc) == -1) {
        exit(1);
    }

    pipefs_read_config();

    return fuse_main(args.argc, args.argv, &pipefs_oper, NULL);
}
