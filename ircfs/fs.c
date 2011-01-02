/*
 * mattias@sudac.org
 *
 * TODO: rename function?
 * TODO: error handling and reporting
 * TODO: symlinks, just store a path name in entry? extend fs_entry struct
 *
 */


#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>

#include "fs.h"


fs_entry *fs_create_entry(char *name, mode_t mode)
{
    fs_entry *e;

    /* alloc and set all pointers to NULL */
    e = g_new0(fs_entry, 1);
    e->name = g_strdup(name);
    e->stat.st_mode = mode;

    if(mode & S_IFDIR)
        e->dir = g_hash_table_new((GHashFunc)g_str_hash,
                                  (GEqualFunc)g_str_equal);

    return e;
}

static void fs_delete_entry_recurse(fs_entry *entry);

static void fs_delete_entry_foreach_aux(gpointer key, gpointer value, gpointer data)
{
    fs_delete_entry_recurse((fs_entry*)value);
}

static void fs_delete_entry_recurse(fs_entry *entry)
{
    if(entry->dir != NULL)
    {
        g_hash_table_foreach(entry->dir,
                             fs_delete_entry_foreach_aux,
                             NULL);
        
        g_hash_table_destroy(entry->dir);
    }

    g_free(entry->name);
    g_free(entry);
}

void fs_delete_entry(fs_entry *entry)
{
    if(entry->parent == NULL)
        fprintf(stderr, "Warning: deleting non-attached entry \"%s\"\n",
                entry->name);
    else
    {
        printf("deleting %s from %s\n", entry->name, entry->parent->name);
        g_hash_table_remove(entry->parent->dir, entry->name);
    }

    fs_delete_entry_recurse(entry);
}

void fs_attach_entry(fs_entry *parent, fs_entry *entry)
{
    if(parent->dir == NULL)
    {
        fprintf(stderr, "Error: attaching entry \"%s\" to non-dir entry \"%s\"\n",
                entry->name, parent->name);

        return;
    }
    
    entry->parent = parent;
    g_hash_table_insert(parent->dir, entry->name, entry); 
}

void fs_entry_rename(fs_entry *entry, char *name)
{
    char *new;

    new = g_strdup(name);

    if(entry->parent != NULL)
    {
        g_hash_table_remove(entry->parent->dir, entry->name);
        g_hash_table_insert(entry->parent->dir, new, entry);
    }

    g_free(entry->name);
    entry->name = new;
}

fs_entry *fs_lookup(fs_entry *root, char *path)
{
    fs_entry *e;
    char *copy, *temp, *s;

    if(path[0] != '/')
    {
        fprintf(stderr, "Error: non-absoulte path lookup \"%s\"\n",
                path);

        return NULL;
    }

    /* "/" case */
    if(path[1] == '\0')
        return root;
   
    /* +1 to skip first "/" */
    copy = g_strdup(path + 1);
    temp = copy;
    e = root;
    
    while(TRUE)
    {
        s = strsep(&temp, "/");
        if(s == NULL) /* whole path done, ok */
            break;

        printf("s=\"%s\" temp=\"%s\"\n", s, temp);

        /* is non-dir and path not done? fail */
        if(e->dir == NULL)
        {
            e = NULL;
            
            break;
        }
        
        /* lookup and overwrite e for next iteration */
        e = g_hash_table_lookup(e->dir, s);
        if(e == NULL) /* path part not found, fail */
            break;
    }
    
    g_free(copy);

    return e;
}

static void fs_dump_tree_aux(fs_entry *entry, int depth);

static void fs_dump_tree_foreach_aux(gpointer key, gpointer value, gpointer data)
{
    fs_dump_tree_aux((fs_entry*)value, GPOINTER_TO_INT(data));
}

static void fs_dump_tree_aux(fs_entry *entry, int depth)
{
    int i;

    for(i = 0; i < depth; i++)
        fprintf(stderr, "    ");
    fprintf(stderr, "%s \"%s\" %p parent=%p\n",
            (entry->dir ? "DIR" : "FILE"),
            entry->name,
            entry,
            entry->parent);

    if(entry->dir != NULL)
        g_hash_table_foreach(entry->dir,
                             fs_dump_tree_foreach_aux,
                             GINT_TO_POINTER(depth + 1));
}

void fs_dump_tree(fs_entry *entry)
{
    fs_dump_tree_aux(entry, 0);
}

