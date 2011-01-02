#ifndef __NAMESPACE_H__
#define __NAMESPACE_H__

#include <unistd.h>
#include <sys/stat.h>
#include <glib.h>

typedef struct namespace_entry namespace_entry;

struct namespace_entry
{
    char *name;
    namespace_entry *parent;
    GHashTable *dir; /* only for directorie entries */
    struct stat stat;
    int links;
    char *data;
    off_t length;
};


namespace_entry *namespace_new(const char *name, mode_t mode, const char *link);
void namespace_delete(namespace_entry *entry);
void namespace_attach(namespace_entry *parent, namespace_entry *entry);
void namespace_detach(namespace_entry *e);
void namespace_rename(namespace_entry *entry, const char *name);
namespace_entry *namespace_lookup(namespace_entry *root, const char *path);

void namespace_debug_tree(namespace_entry *entry);

#endif

