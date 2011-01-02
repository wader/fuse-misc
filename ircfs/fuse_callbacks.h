#ifndef __FUSE_CALLBACKS_H__
#define __FUSE_CALLBACKS_H__

#include <fuse.h>

int ircfs_getattr(const char *path, struct stat *stbuf);
int ircfs_getdir(const char *path, fuse_dirh_t h, fuse_dirfil_t filler);
int ircfs_mkdir(const char *path, mode_t mode);
int ircfs_rmdir(const char *path);
int ircfs_statfs(const char *path, struct statfs *st);
int ircfs_open(const char *path, int flags);
int ircfs_read(const char *path, char *buf, size_t size, off_t offset);
int ircfs_write(const char *path, const char *buf, size_t size, off_t offset);
int ircfs_unlink(const char *path);
int ircfs_truncate(const char *path, off_t size);

#endif

