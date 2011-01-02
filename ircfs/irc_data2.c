



GList *channel_list;
GHashTable *channel_hash;
GList *user_list;
GHashTable *user_hash;


struct _channel_user
{
    user *user_ref;
    gboolean op;
    gboolean halfop;
    gboolean voice;
}

struct _channel
{
    GString *name;
    GString *topic;

    /* bans, modes */

    GList *user_list;
    GHashTable *user_hash;
}

struct _user
{
    GString *name;
    user_whois; *whois;
};


struct _user_whois
{
    GString *host;
    GString *user;
    GString *realname;
    GString *operator /* 0 or 1 ? */
    GString *server;
    GList *channels; // WHOISCHANNELS may appeare many times, take care...
};


int irc_data_init()
{
    channel_list = NULL;
    channel_hash = g_hash_table_new((GHashFunc)g_str_hash, (GEqualFunc)g_str_equal);
    user_list = NULL;
    user_hash = g_hash_table_new((GHashFunc)g_str_hash, (GEqualFunc)g_str_equal);
}


user *user_add(char *name)
{
    user *u;

    u = g_new(user, 1);

    
}

int user_del(char *name)
{
}

user *user_find(char *name)
{
}

channel *channel_add(char *name)
{
    channel *c = NULL;

    c = g_new(channel, 1);
    c->name = g_string_new(name);
    c->topic = 
    c->user_list = NULL;
    c->user_hash = g_hash_table_new((GHashFunc)g_str_hash, (GEqualFunc)g_str_equal);
}

channel_user *channel_user_add(channel *c, user *u)
{
    channel_user *cu = NULL;

    cu = g_new(channel_user, 1);
    cu->user_ref = u;
    cu->op = FALSE;
    cu->halfop = FALSE;
    cu->voice = FALSE;

    g_hash_table_insert(c->user_hash, 

    return cu;
}



