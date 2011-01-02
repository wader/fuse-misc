#ifndef __IRC_DATA_H__
#define __IRC_DATA_H__

#include <time.h>
#include <glib.h>

GList *channels;
GString *selfname;
GString *motd;
G_LOCK_EXTERN(data);

struct _user
{
    GString *name;
    gboolean op;
    gboolean halfop;
    gboolean voice;
    time_t mtime;
};

typedef struct _user user;

struct _channel
{
    GString *name;
    GString *topic;
    GList *users;
    time_t mtime;
};

typedef struct _channel channel;


int irc_data_init();
void irc_data_free();
user *user_find(channel *c, char *name);
user *user_add(channel *c, char *name);
int user_del(channel *c, char *name);
channel *channel_find(char *name);
channel *channel_add(char *name);
int channel_del(char *name);
    
#endif
