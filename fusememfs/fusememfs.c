/*
 * FUSE memory filesystem
 *
 * Copyright (C)2004 Mattias Wadman <mattias@sudac.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 * TODO: many hard links, needs refcounting etc... puh
 * TODO: make more time updates, ctime on dir when adding a file?
 * TODO: use fuse_loop_mt, need locks
 * TODO: stats: links, block size... fill stat struct (getattr)
 * TODO: cleanup and make rename smarter
 * TODO: use fuse_main, no need for own fork code etc, not safe yet (multithreding)
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <getopt.h>
#include <stdarg.h>
#include <signal.h>
#include <errno.h>
#include <dirent.h>
#include <libgen.h>
#include <time.h>
#include <fuse.h>
#include <glib.h>

#include "config.h"
#include "namespace.h"


static struct fuse *fuse;
static int fuse_fd;
static char *fuse_mountpoint;
static namespace_entry *root;
static int debug;


/* lookup parent entry (always a directory) */
static namespace_entry *parent_entry(const char *path)
{
    namespace_entry *e;
    char *c, *s;
    
    c = g_strdup(path);
    s = dirname(c);
    e = namespace_lookup(root, s);
    g_free(c);

    return e;
}

/* return a allocated basename string */
static char *get_basename(const char *path)
{
    char *c, *s, *r;

    c = g_strdup(path);
    s = basename(c);
    r = g_strdup(s);
    g_free(c);

    return r;
}

static int fusememfs_getattr(const char *path, struct stat *stbuf)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "getattr path=\"%s\" stbuf=%p\n", path, stbuf);
    
    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;
    
    memcpy(stbuf, &e->stat, sizeof(struct stat));
   
    stbuf->st_size = e->length;
    stbuf->st_nlink = e->links;
    
    return 0;
}

static int fusememfs_readlink(const char *path, char *buf, size_t size)
{
    namespace_entry *e;

    if(debug)
        fprintf(stderr, "readlink path=\"%s\" buf=%p size=%d\n",
                path, buf, size);
    
    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir != NULL)
        return -EISDIR;
    
    if(!S_ISLNK(e->stat.st_mode))
        return -EINVAL;

    if(e->length > size)
        return -ENAMETOOLONG; /* FIXME: ENOMEM or EFAULT insted? */
    
    e->stat.st_atime = time(NULL);
    
    memcpy(buf, e->data, size);
    
    return 0;
}

struct getdir_dummy
{
    fuse_dirh_t h;
    fuse_dirfil_t filler;
};

static void fusememfs_getdir_aux(gpointer key, gpointer value, gpointer data)
{
    namespace_entry *e;
    struct getdir_dummy *d;

    e = (namespace_entry*)value;
    d = (struct getdir_dummy*)data;

    /* IFTODT converts stat mode to dir type */
    d->filler(d->h, e->name, IFTODT(e->stat.st_mode));
}

static int fusememfs_getdir(const char *path, fuse_dirh_t h, fuse_dirfil_t filler)
{
    namespace_entry *e;
    struct getdir_dummy d;
    
    if(debug)
        fprintf(stderr, "getdir path=\"%s\"\n", path);

    d.h = h;
    d.filler = filler;
    
    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir == NULL)
        return -ENOTDIR;

    /* for visual apperence only :) */
    filler(h, ".", DT_DIR);
    filler(h, "..", DT_DIR);
    
    g_hash_table_foreach(e->dir,
                         fusememfs_getdir_aux,
                         &d);

    return 0;
}

static int fusememfs_mknod(const char *path, mode_t mode, dev_t rdev)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "mknod path=\"%s\" mode=0%o rdev=%x\n",
                path, mode, (unsigned int)rdev);

    /* make sure not to create directories or links with mknod */
    if(S_ISDIR(mode) || S_ISLNK(mode))
        return -EINVAL;
    
    e = namespace_lookup(root, path);
    if(e == NULL)
    {
        char *b;
        namespace_entry *n;

        e = parent_entry(path);
        if(e == NULL)
            return -ENOENT;

        b = get_basename(path);
        n = namespace_new(b, mode, NULL);
        g_free(b);
        namespace_attach(e, n);
        n->stat.st_mode = mode;
        n->stat.st_atime = 
        n->stat.st_mtime = 
        n->stat.st_ctime = time(NULL);
        n->stat.st_uid = getuid();
        n->stat.st_gid = getgid();
        /* Not needed? done in namespace_new (g_new0)
        n->length = 0;
        n->data = NULL;
        */
    }
    else
        return -EEXIST;

    return 0;
}

static int fusememfs_mkdir(const char *path, mode_t mode)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "mkdir path=\"%s\" mode=0%o\n", path, mode);

    e = namespace_lookup(root, path);
    if(e == NULL)
    {
        char *b;
        namespace_entry *n;

        e = parent_entry(path);
        if(e == NULL)
            return -ENOENT;

        b = get_basename(path);
        /* force DIR flag, makes sure namespace_new creates hash table */
        n = namespace_new(b, S_IFDIR | mode, NULL);
        g_free(b);
        namespace_attach(e, n);
        n->stat.st_atime = 
        n->stat.st_mtime = 
        n->stat.st_ctime = time(NULL);
        n->stat.st_uid = getuid();
        n->stat.st_gid = getgid();
    }
    else
        return -EEXIST;

    return 0;
}

static int fusememfs_unlink(const char *path)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "unlink path=\"%s\"\n", path);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir != NULL)
        return -EISDIR;

    namespace_delete(e);

    return 0;
}

static int fusememfs_rmdir(const char *path)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "rmdir path=\"%s\"\n", path);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir == NULL)
        return -ENOTDIR;

    if(g_hash_table_size(e->dir) > 0)
        return -ENOTEMPTY;

    namespace_delete(e);

    return 0;
}

static int fusememfs_symlink(const char *from, const char *to)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "symlink from=\"%s\" to=\"%s\"\n", from, to);

    e = namespace_lookup(root, to);
    if(e == NULL)
    {
        char *b;
        namespace_entry *n;

        e = parent_entry(to);
        if(e == NULL)
            return -ENOENT;

        b = get_basename(to);
        /* force LNK flag, makes namespace_new creates a link */
        n = namespace_new(b, S_IFLNK | 0777, from);
        g_free(b);
        namespace_attach(e, n);
        n->stat.st_atime = 
        n->stat.st_mtime = 
        n->stat.st_ctime = time(NULL);
        n->stat.st_uid = getuid();
        n->stat.st_gid = getgid();
    }
    else
        return -EEXIST;

    return 0;
}

static int fusememfs_rename(const char *from, const char *to)
{
    namespace_entry *e, *t, *p;
    char *b;
    
    if(debug)
        fprintf(stderr, "symlink from=\"%s\" to=\"%s\"\n", from, to);

    e = namespace_lookup(root, from);
    if(e == NULL)
        return -ENOENT;
    
    t = namespace_lookup(root, to);
    p = parent_entry(to);
   
    /* whatever to non-existing, rename/change location */
    if(t == NULL)
    {
        if(p == NULL)
            return -ENOENT;

        if(p->dir == NULL)
            return -ENOENT;

        //if(e->dir != NULL t->dir != NULL &&
        //   g_hash_table_size(t) > 0)
        //    return -ENOTEMPTY;

        namespace_detach(e);
        b = get_basename(to);
        namespace_rename(e, b);
        g_free(b);
        namespace_attach(p, e);
    }
    /* whatever to dir, change location */
    else if(t->dir != NULL)
    {
        namespace_detach(e);
        namespace_attach(t, e);
    }
    /* file to a file, overwrite */
    else if(e->dir == NULL)
    {
        namespace_detach(e);
        namespace_rename(e, t->name);
        namespace_delete(t);
        namespace_attach(p, e);
    }
        
    e->stat.st_ctime = time(NULL);
    
    return 0;
}

static int fusememfs_link(const char *from, const char *to)
{
    if(debug)
        fprintf(stderr, "link from=\"%s\" to=\"%s\"\n", from, to);
    
    return -ENOSYS;
}

static int fusememfs_chmod(const char *path, mode_t mode)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "chmod path=\"%s\" mode=0%o\n", path, mode);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    e->stat.st_mode = mode;
    e->stat.st_ctime = time(NULL);

    return 0;
}

static int fusememfs_chown(const char *path, uid_t uid, gid_t gid)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "chown path=\"%s\" uid=%d gid=%d\n", path, uid, gid);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    e->stat.st_uid = uid;
    e->stat.st_gid = gid;
    e->stat.st_ctime = time(NULL);

    return 0;
}

static int fusememfs_truncate(const char *path, off_t size)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "truncate path=\"%s\" size=%lld\n", path, size);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;
    
    if(e->dir != NULL)
        return -EISDIR;

    e->length = size;
    e->data = g_realloc(e->data, size);
    e->stat.st_mtime = time(NULL);

    return 0;
}

static int fusememfs_utime(const char *path, struct utimbuf *buf)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "utime path=\"%s\" buf->actime=%ld buf->modtime=%ld\n",
                path, buf->actime, buf->modtime);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    e->stat.st_atime = buf->actime;
    e->stat.st_mtime = buf->modtime;
    
    return 0;
}

static int fusememfs_open(const char *path, int flags)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "open path=\"%s\" flags=%x\n", path, flags);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir != NULL)
        return -EISDIR;

    return 0;
}

static int fusememfs_read(const char *path, char *buf, size_t size, off_t offset)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "read path=\"%s\" buf=%p size=%d offset=%lld\n",
                path, buf, size, offset);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir != NULL)
        return -EISDIR;

    offset = CLAMP(offset, 0, e->length);
    size = CLAMP(size, 0, e->length - offset);
    e->stat.st_atime = time(NULL);
    
    memcpy(buf, e->data + offset, size); 
    
    return size;
}

static int fusememfs_write(const char *path, const char *buf, size_t size,
                     off_t offset)
{
    namespace_entry *e;
    
    if(debug)
        fprintf(stderr, "write path=\"%s\" buf=%p size=%d offset=%lld\n",
                path, buf, size, offset);

    e = namespace_lookup(root, path);
    if(e == NULL)
        return -ENOENT;

    if(e->dir != NULL)
        return -EISDIR;
    
    if(offset + size > e->length)
    {
        e->length = offset + size;
        e->data = g_realloc(e->data, e->length);
    }
    
    e->stat.st_mtime = time(NULL);
    
    memcpy(e->data + offset, buf, size);

    return size;
}

static int fusememfs_statfs(const char *path, struct statfs *stbuf)
{
    if(debug)
        fprintf(stderr, "statfs path=\"%s\" stbuf=%p\n", path, stbuf);
    
    return -ENOSYS;
}

static int fusememfs_release(const char *path, int flags)
{
    if(debug)
        fprintf(stderr, "release path=\"%s\" flags=%x\n", path, flags);
    
    return 0;
}

static int fusememfs_fsync(const char *path, int isdatasync)
{
    if(debug)
        fprintf(stderr, "fsync path=\"%s\" isdatasync=%d\n",
                path, isdatasync);
    
    return 0;
}

static struct fuse_operations fusememfs_op =
{
    .getattr    = fusememfs_getattr,
    .readlink   = fusememfs_readlink,
    .getdir     = fusememfs_getdir,
    .mknod      = fusememfs_mknod,
    .mkdir      = fusememfs_mkdir,
    .symlink    = fusememfs_symlink,
    .unlink     = fusememfs_unlink,
    .rmdir      = fusememfs_rmdir,
    .rename     = fusememfs_rename,
    .link       = fusememfs_link,
    .chmod      = fusememfs_chmod,
    .chown      = fusememfs_chown,
    .truncate   = fusememfs_truncate,
    .utime      = fusememfs_utime,
    .open       = fusememfs_open,
    .read       = fusememfs_read,
    .write      = fusememfs_write,
    .statfs     = fusememfs_statfs,
    .release    = fusememfs_release,
    .fsync      = fusememfs_fsync,
};

static void help(char *program)
{
    fprintf(stderr,
            PACKAGE_NAME " " PACKAGE_VERSION "\n"
            "Usage: %s [-bdf] MOUNTPOINT\n"
            "    -b  Dont run in background\n"
            "    -d  Show all filesystem calls\n"
            "    -f  Enable fuse debug\n",
            program
            );
}

static void die(char *format, ...)
{
    va_list args;
    
    fprintf(stderr, "Exit: ");
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    exit(EXIT_FAILURE);
}

static void signal_handler(int signal)
{
    fprintf(stderr, "Got terminate signal\n");
    
    if(fuse != NULL)
        fuse_exit(fuse);
}

static void dump_tree(int signal)
{
    namespace_debug_tree(root);
}

void daemonize()
{
    int fd;
    
    switch(fork())
    {
        case -1: /* error */
            perror("fork");
            exit(EXIT_FAILURE);
            break;
        case 0: /* child */
            break;
        default: /* parent */
            exit(EXIT_SUCCESS);
            break;
    }
    
    /* set stdin, stdout and stderr to /dev/null */
    fd = open("/dev/null", O_RDWR);
    dup2(fd, STDIN_FILENO);
    dup2(fd, STDOUT_FILENO);
    dup2(fd, STDERR_FILENO);
    if(fd != STDIN_FILENO && fd != STDOUT_FILENO && fd != STDERR_FILENO)
        close(fd);
    
    setsid(); /* dont recive signals from parent */
    chdir("/"); /* dont disturb mounts */
}

int main(int argc, char **argv)
{
    char *working_dir;
    char *fusemount_args[] =
    {
        "-n", "fusememfs", /* filesystem name */
        NULL
    };
    int fuse_flags;
    int getopt_c;
    int background;
 
    fuse_flags = 0;
    background = 1;
    debug = 0;
    working_dir = get_current_dir_name();
    if(working_dir == NULL)
        die("get_current_dir_name failed for working_dir\n");
    fuse = NULL;
    fuse_fd = -1;
    root = namespace_new("", S_IFDIR | 0755, NULL);

    for(;;)
    {
        getopt_c = getopt(argc, argv, "bdf");

        if(getopt_c == -1)
            break;
        else if(getopt_c == 'b')
            background = 0;
        else if(getopt_c == 'd')
            debug = 1;
        else if(getopt_c == 'f')
            fuse_flags |= FUSE_DEBUG;
        else
            exit(EXIT_FAILURE);
    }

    if(optind != argc - 1)
    {
        help(argv[0]);

        exit(EXIT_FAILURE);
    }

    if(argv[optind][0] == '/')
        fuse_mountpoint = argv[optind];
    else
    {
        asprintf(&fuse_mountpoint, "%s%s%s",
                 working_dir,
                 /* avoid doubel slash, get_c... can end with a slash, see man page about $PWD */
                 (working_dir[strlen(working_dir) - 1] == '/' ? "" : "/"),
                 argv[optind]
                 );
        if(fuse_mountpoint == NULL)
            die("asprintf fuse_mountpoint failed\n");
    }
  
    signal(SIGINT, signal_handler);
    signal(SIGHUP, signal_handler);
    signal(SIGTERM, signal_handler);
    signal(SIGUSR1, dump_tree);
    
    fuse_fd = fuse_mount(fuse_mountpoint, (const char **)fusemount_args);
    if(fuse_fd == -1)
        die("fuse_mount failed\n");
    fuse = fuse_new(fuse_fd, fuse_flags, &fusememfs_op);
    if(fuse == NULL)
        die("fuse_new failed\n");
   
    if(background)
        daemonize();
    
    fuse_loop(fuse);
        
    fuse_destroy(fuse);
    fuse_unmount(fuse_mountpoint);

    return EXIT_SUCCESS;
}

