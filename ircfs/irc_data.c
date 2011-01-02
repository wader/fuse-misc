/*
 * ircfs
 *
 * mattias@sudac.org
 */

#include <time.h>
#include <string.h>
#include <glib.h>

#include "irc_data.h"


G_LOCK_DEFINE(data);


int irc_data_init()
{
    channels = NULL;
    motd = NULL;
    selfname = NULL;

    return TRUE;
}

void irc_data_free()
{
    if(motd != NULL)
        g_string_free(motd, TRUE);
    if(selfname != NULL)
        g_string_free(selfname, TRUE);

    /* TODO: g_list_free(channels)? */
}

static gint channel_compare(gconstpointer data, gconstpointer name)
{
    return strcmp(((channel*)data)->name->str, (char*)name);
}

static gint user_compare(gconstpointer data, gconstpointer name)
{
    return strcmp(((user*)data)->name->str, (char*)name);
}

static void user_free(user *u)
{
    g_string_free(u->name, TRUE);
    g_free(u);
}

user *user_find(channel *c, char *name)
{
    GList *l = NULL;
        
    l = g_list_find_custom(c->users, name, user_compare);
    if(l != NULL)
        return l->data;
    else
        return NULL;
}

user *user_add(channel *c, char *name)
{
    user *u = NULL;

    u = g_new(user, 1);
    u->name = g_string_new(name);
    u->op = FALSE;
    u->halfop = FALSE;
    u->voice = FALSE;
    u->mtime = time(NULL);

    c->mtime = time(NULL);
    c->users = g_list_prepend(c->users, u);
    
    return u;
}

//int user_channel_del(channel *c, char *name)
int user_del(channel *c, char *name)
{
    user *u = NULL;

    u = user_find(c, name);
    if(u == NULL)
        return FALSE;

    c->users = g_list_remove(c->users, u);
    c->mtime = time(NULL);
    user_free(u);
    
    return TRUE;
}

/*
int user_del(char *name)
{
    GList *l = NULL;
    gboolean s = FALSE;

    for(l = channels; l; l = l->next)
        if(user_channel_del((channel*)l->data, name) == TRUE)
            s = TRUE;
    
    return s;
}
*/

static void channel_free_aux(gpointer data, gpointer user_data)
{
    user_free((user*)data);
}

static void channel_free(channel *c)
{
    g_list_foreach(c->users, channel_free_aux, NULL);
    g_string_free(c->name, TRUE);
    g_string_free(c->topic, TRUE);
    g_free(c);
}

channel *channel_find(char *name)
{
    GList *l = NULL;
        
    l = g_list_find_custom(channels, name, channel_compare);
    if(l != NULL)
        return l->data;
    else
        return NULL;
}

channel *channel_add(char *name)
{
    channel *c = NULL;

    c = g_new(channel, 1);
    c->name = g_string_new(name);
    c->users = NULL;
    c->topic = g_string_new("");
    c->mtime = time(NULL);

    channels = g_list_prepend(channels, c);

    return c;
}

int channel_del(char *name)
{
    channel *c;
    
    c = channel_find(name);
    if(c == NULL)
        return FALSE;
    
    channels = g_list_remove(channels, c);
    channel_free(c);

    /* TODO: time update for root? */

    return TRUE;
}

