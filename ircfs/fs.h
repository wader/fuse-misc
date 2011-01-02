#ifndef __FS_H__
#define __FS_H__

#include <unistd.h>
#include <fuse.h>
#include <glib.h>

typedef struct fs_entry fs_entry;

struct fs_entry
{
    char *name;
    fs_entry *parent;
    struct stat stat;
    struct fuse_operations op;
    GHashTable *dir; /* only for directories */
    void *data; /* TODO: user data */
};


fs_entry *fs_create_entry(char *name, mode_t mode);
void fs_delete_entry(fs_entry *entry);
void fs_attach_entry(fs_entry *parent, fs_entry *entry);
void fs_entry_rename(fs_entry *entry, char *name);
fs_entry *fs_lookup(fs_entry *root, char *path);
void fs_dump_tree(fs_entry *entry);

#endif

