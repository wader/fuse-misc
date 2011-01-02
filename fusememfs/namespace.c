/*
 * Help functions for creating namespaces
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
 * TODO: error handling and reporting
 *
 */


#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <glib.h>

#include "namespace.h"


namespace_entry *namespace_new(const char *name, mode_t mode, const char *link)
{
    namespace_entry *e;

    /* alloc and set all pointers to NULL */
    e = g_new0(namespace_entry, 1);
    e->name = g_strdup(name);
    e->stat.st_mode = mode;

    if(S_ISDIR(mode))
    {
        e->dir = g_hash_table_new((GHashFunc)g_str_hash,
                                  (GEqualFunc)g_str_equal);
        e->links = 1; /* FIXME: "." self reference? */
    }
    else if(S_ISLNK(mode))
    {
        if(link == NULL)
            fprintf(stderr,
                    "namespace: error: new symlink \"%s\" has NULL link\n", name);
        else
        {
            e->data = g_strdup(link);
            e->length = strlen(link);
        }
    }
    
    return e;
}

static void namespace_delete_recurse(namespace_entry *entry);

static void namespace_delete_foreach(gpointer key, gpointer value, gpointer data)
{
    namespace_delete_recurse((namespace_entry*)value);
}

static void namespace_delete_recurse(namespace_entry *entry)
{
    if(entry->dir != NULL)
    {
        if(g_hash_table_size(entry->dir) > 0)
            g_hash_table_foreach(entry->dir,
                                 namespace_delete_foreach,
                                 NULL);
        
        g_hash_table_destroy(entry->dir);
    }

    g_free(entry->name);
    g_free(entry->data);
    g_free(entry);
}

void namespace_delete(namespace_entry *entry)
{
    if(entry->parent == NULL)
        fprintf(stderr,
                "namespace: warning: delete non-attached \"%s\"\n",
                entry->name);
    else
    {
        g_hash_table_remove(entry->parent->dir, entry->name);
        if(entry->dir != NULL)
            entry->parent->links--;
    }

    namespace_delete_recurse(entry);
}

void namespace_attach(namespace_entry *parent, namespace_entry *entry)
{
    if(parent->dir == NULL)
    {
        fprintf(stderr,
                "namespace: error: attach \"%s\" to non-dir \"%s\"\n",
                entry->name, parent->name);

        return;
    }

    g_hash_table_insert(parent->dir, entry->name, entry);
    entry->parent = parent;
    entry->links++;
    if(entry->dir != NULL)
        parent->links++;
}

void namespace_detach(namespace_entry *entry)
{
    if(entry->parent == NULL)
    {
        fprintf(stderr, "namespace: error: detach non-attached \"%s\"\n",
                entry->name);
    
        return;
    }
    
    g_hash_table_remove(entry->parent->dir, entry->name);
    if(entry->dir != NULL)
        entry->parent->links--;
    entry->parent = NULL;
    entry->links--;
}

void namespace_rename(namespace_entry *entry, const char *name)
{
    char *new;

    new = g_strdup(name);

    if(entry->parent != NULL)
    {
        g_hash_table_remove(entry->parent->dir, entry->name);
        /* use newly allocated string as hash key */
        g_hash_table_insert(entry->parent->dir, new, entry);
    }

    g_free(entry->name);
    entry->name = new;
}

namespace_entry *namespace_lookup(namespace_entry *root, const char *path)
{
    namespace_entry *e;
    char *c, *t, *s;

    if(path[0] != '/')
    {
        fprintf(stderr, "namespace: error: lookup non-absoulte path \"%s\"\n",
                path);

        return NULL;
    }

    /* "/" case */
    if(path[1] == '\0')
        return root;
   
    /* +1 to skip first "/" */
    c = g_strdup(path + 1);
    t = c;
    e = root;
    
    while(TRUE)
    {
        s = strsep(&t, "/");
        if(s == NULL) /* whole path done, ok */
            break;
        
        /* printf("namespace: debug: lookup part=\"%s\" next=\"%s\" cur=\"%s\"\n",
                s, t, e->name); */

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
    
    g_free(c);

    return e;
}

static void namespace_debug_tree_aux(namespace_entry *entry, int depth);

static void namespace_debug_tree_foreach(gpointer key, gpointer value, gpointer data)
{
    namespace_debug_tree_aux((namespace_entry*)value, GPOINTER_TO_INT(data));
}

static void namespace_debug_tree_aux(namespace_entry *entry, int depth)
{
    int i;

    for(i = 0; i < depth; i++)
        fprintf(stderr, "    ");
    fprintf(stderr, "%s (type=%s dir=%p data=%s(%lld))\n",
            entry->name,
            (S_ISDIR(entry->stat.st_mode) ? "DIR" :
            (S_ISREG(entry->stat.st_mode) ? "REG" :
            (S_ISLNK(entry->stat.st_mode) ? "SYMLINK" : "UNKNOWN"))),
            entry->dir,
            (S_ISLNK(entry->stat.st_mode) ? entry->data : "<data>"),
            entry->length);

    if(entry->dir != NULL)
        g_hash_table_foreach(entry->dir,
                             namespace_debug_tree_foreach,
                             GINT_TO_POINTER(depth + 1));
}

void namespace_debug_tree(namespace_entry *entry)
{
    namespace_debug_tree_aux(entry, 0);
}

