/*
 * ircfs
 *
 * mattias@sudac.org
 */

#include <stdio.h>
#include <string.h>
#include <glib.h>

#include "irc.h"
#include "ircfs.h"
#include "irc_data.h"


void irc_callback_debug(irc_connection *connection, char *command, GString **p, int n)
{
    int i;

    if(log_types[LOG_DEBUG] != 0)
    {
        LOG(LOG_DEBUG, ""); /* prints function name */
        for(i = 0; i < n; i++)
            fprintf(stderr, "%d=\"%s\"(%d) ", i, p[i]->str, p[i]->len);
        fprintf(stderr, "\n");
    }
}

void irc_callback_talk_push(irc_connection *ic, char *command, GString **p, int n)
{
    TALK_PUSH(command);
}

void irc_callback_motd_start(irc_connection *ic, char *command, GString **p, int n)
{
    LOCK(data);
   
    if(motd == NULL)
    {
        LOG(LOG_DEBUG, "new\n");
        motd = g_string_new("");
    }
    else
    {
        LOG(LOG_DEBUG, "truncate\n");
        g_string_truncate(motd, 0);
    }
    
    UNLOCK(data);
}

/* :- <string> */
void irc_callback_motd(irc_connection *ic, char *command, GString **p, int n)
{
    if(n < 4)
    {
        LOG(LOG_ERROR, "%d < 4 failed\n", n);

        return;
    }
    
    LOCK(data);

    if(motd == NULL)
        LOG(LOG_ERROR, "motd = NULL, motd before motd_start?\n");
    else
    {
        if(p[3]->len > 1)
        {
            LOG(LOG_DEBUG, "concat line=\"%s\"", p[3]->str + 2);
            /* +2 to skip "- " */
            g_string_append(motd, p[3]->str + 2);
            g_string_append_c(motd, '\n');
        }
        else
            LOG(LOG_ERROR, "malformed line=\"%s\"", p[3]->str);
    }
    
    UNLOCK(data);
}

/* ( '=' / '*' / '@' ) <channel> ' ' : [ '@' / '+' ] <nick> *( ' ' [ '@' / '+' ] <nick> ) */
void irc_callback_names_reply(irc_connection *ic, char *command, GString **p, int n)
{
    char *w, *s;
    channel *c;
    user *u;
    
    if(n < 6)
    {
        LOG(LOG_ERROR, "%d < 6 failed\n", n);
        
        return;
    }

    LOCK(data);
    
    c = channel_find(p[4]->str);
    
    if(c != NULL)
    {
        if(p[3]->str[0] == '=')
        {
            /* public channel */
        }
        else if(p[3]->str[0] == '@')
        {
            /* secret channel */
        }
        else if(p[3]->str[0] == '*')
        {
            /* private channel */
        }
        else
            LOG(LOG_ERROR, "unknown channel flag %c\n", p[3]->str[0]);

        
        s = w = p[5]->str;
        
        while(w != NULL)
        {
            s = strchr(s, ' ');
            if(s != NULL)
                *s++ = '\0';
            
            if(strlen(w) > 0) /* fix for trailing space in nick list */
            {
                if(user_find(c, w + (irc_is_mode(*w) ? 1 : 0)) == NULL)
                {
                    u = user_add(c, w + (irc_is_mode(*w) ? 1 : 0));
                    u->op = (*w == '@' ? TRUE : FALSE);
                    u->halfop = (*w == '%' ? TRUE : FALSE);
                    u->voice = (*w == '+' ? TRUE : FALSE);
                    LOG(LOG_DEBUG, "adding channel=%s user=%s op=%d halfop=%d voice=%d\n", p[4]->str, u->name->str, u->op, u->halfop, u->voice);
                }
                else
                    LOG(LOG_ERROR, "user already added channel=%s user=%s\n", p[4]->str, w);
            }

            w = s;
        }
    }
    
    UNLOCK(data);
}

void irc_callback_topic_reply(irc_connection *ic, char *command, GString **p, int n)
{
    channel *c;
    
    if(n < 5)
    {
        LOG(LOG_ERROR, "%d < 5 failed\n", n);
        
        return;
    }
    
    LOG(LOG_DEBUG, "channel=%s topic=%s\n", p[3]->str, p[4]->str);

    LOCK(data);

    c = channel_find(p[3]->str);
    if(c == NULL)
        LOG(LOG_ERROR, "topic reply for unknown channel\n");
    else
    {
        LOG(LOG_DEBUG, "topic reply ok\n");
        g_string_assign(c->topic, p[4]->str);
        g_string_append_c(c->topic, '\n');
    }

    UNLOCK(data);
}

void irc_callback_topic(irc_connection *ic, char *command, GString **p, int n)
{
    channel *c = NULL;
    char *from = NULL;
    
    if(n < 4)
    {
        LOG(LOG_ERROR, "%d < 4 failed\n", n);
        
        return;
    }
    
    from = irc_prefix_nickname(p[0]->str);
    LOG(LOG_DEBUG, "channel=%s from=%s topic=%s\n", p[2]->str, from, p[3]->str);

    LOCK(data);
    
    c = channel_find(p[2]->str);
    if(c == NULL)
        LOG(LOG_ERROR, "topic reply for unknown channel\n");
    else
    {
        LOG(LOG_DEBUG, "topic reply ok\n");
        g_string_assign(c->topic, p[3]->str);
        g_string_append_c(c->topic, '\n');
    }

    /* if TOPIC was from us a thread might be waiting */
    if(strcmp(from, selfname->str) == 0)
        TALK_PUSH("TOPIC");

    UNLOCK(data);
}

void irc_callback_ping(irc_connection *ic, char *command, GString **p, int n)
{
    if(n < 3)
    {
        LOG(LOG_ERROR, "%d < 3 failed\n", n);
        
        return;
    }
    
    irc_send(ic, "PONG %s", p[2]->str);
}

void irc_callback_join(irc_connection *ic, char *command, GString **p, int n)
{
    char *from;
    channel *c;
    
    if(n < 3)
    {
        LOG(LOG_ERROR, "%d < 3 failed\n", n);
        
        return;
    }

    from = irc_prefix_nickname(p[0]->str);
    
    LOG(LOG_DEBUG, "channel=%s from=%s\n", p[2]->str, from);

    LOCK(data);
    
    c = channel_find(p[2]->str);
    
    if(strcmp(from, selfname->str) == 0) /* this is us */
    {
        if(c == NULL)
        {
            LOG(LOG_DEBUG, "self join ok\n");
            channel_add(p[2]->str);
        }
        else
            LOG(LOG_ERROR, "self join already unknown channel\n");

        TALK_PUSH("JOIN");
    }
    else /* someone else */
    {
        if(c == NULL)
            LOG(LOG_ERROR, "unknown channel\n");
        else
        {
            LOG(LOG_DEBUG, "join ok\n");
            user_add(c, from);
        }
    }

    UNLOCK(data);
}

void irc_callback_part(irc_connection *ic, char *command, GString **p, int n)
{
    char *from;
    channel *c;
    
    if(n < 3)
    {
        LOG(LOG_ERROR, "%d < 3 failed\n", n);
        
        return;
    }

    from = irc_prefix_nickname(p[0]->str);

    LOG(LOG_DEBUG, "channel=%s from=%s\n", p[2]->str, from);
    
    LOCK(data);
    
    if(strcmp(from, selfname->str) == 0) /* we parted */
    {
        if(channel_del(p[2]->str) == TRUE)
            LOG(LOG_DEBUG, "self part ok\n");
        else
            LOG(LOG_ERROR, "self part unknown channel\n");

        TALK_PUSH("PART");
    }
    else /* someone else parted */
    {
        c = channel_find(p[2]->str);
        
        if(c == NULL)
            LOG(LOG_ERROR, "part unknown channel\n");
        else
        {
            if(user_del(c, from) == TRUE)
                LOG(LOG_DEBUG, "part ok\n");
            else
                LOG(LOG_ERROR, "part unknown user\n");
        }
    }

    UNLOCK(data);
}

/* TODO: if ass TALK_PUSH, only do if MODE message was from us.. */
/* TODO: TALK_PUSH with timeout? */
/* <channel> *( ( "-" / "+" ) *<modes> *<modeparams> ) */
void irc_callback_mode(irc_connection *ic, char *command, GString **p, int n)
{
    int mode_parameter, mode_value = TRUE;
    char *mode;
    channel *c;
    user *u;
    
    if(n < 4)
    {
        LOG(LOG_ERROR, "%d < 4 failed\n", n);
        
        return;
    }
    
    LOCK(data);
    
    c = channel_find(p[2]->str);

    if(c != NULL) /* channel modes */
    {
        c->mtime = time(NULL);
        mode_parameter = 4; /* first possible mode parameter */

        if(p[3]->str[0] == '+' || p[3]->str[0] == '-')
        {
            for(mode = p[3]->str; *mode; mode++)
            {
                if(*mode == '+' || *mode == '-')
                {
                    mode_value = (*mode == '+' ? TRUE : FALSE);

                    continue;
                }
                else if(*mode == 'v' || *mode == 'h' || *mode == 'o')
                {
                    if(!(mode_parameter < n))
                    {
                        LOG(LOG_ERROR, "missing mode parameter for %c\n", *mode);

                        continue;
                    }
                    
                    u = user_find(c, p[mode_parameter]->str);
                    if(u == NULL)
                    {
                        LOG(LOG_ERROR, "channel=%s user=%s unknown user\n", p[2]->str, p[mode_parameter]->str);

                        continue;
                    }
                    
                    if(*mode == 'v')
                        u->voice = mode_value;
                    else if(*mode == 'h')
                        u->halfop = mode_value;
                    else if(*mode == 'o')
                        u->op = mode_value;
        
                    u->mtime = time(NULL);
                    
                    LOG(LOG_DEBUG, "channel=%s user=%s mode=%c set to %d\n", p[2]->str, u->name->str, *mode, mode_value);
                    
                    mode_parameter++;
                }
                else if(*mode == 'l')
                {
                    if(!(mode_parameter < n))
                    {
                        LOG(LOG_ERROR, "missing limit parameter\n");

                        continue;
                    }
                    
                    LOG(LOG_DEBUG, "channel=%s limit=%s\n", p[2]->str, p[mode_parameter]->str);
                    
                    mode_parameter++;
                }
                else if(*mode == 'k')
                {
                    /* TODO: empty key seams to be "*", or? */

                    if(!(mode_parameter < n))
                    {
                        LOG(LOG_ERROR, "missing key parameter\n");

                        continue;
                    }
                    
                    LOG(LOG_DEBUG, "channel=%s key=%s\n", p[2]->str, p[mode_parameter]->str);
                    
                    mode_parameter++;
                }
                else
                    LOG(LOG_ERROR, "unknown channel mode %c\n", *mode);
            }
        }
        else
            LOG(LOG_ERROR, "no +/- prefix mode=%s\n", p[3]->str);
    }
    else /* user modes */
    {
        LOG(LOG_DEBUG, "user mode 2=%s 3=%s\n", p[2]->str, p[3]->str);
    }
    
    UNLOCK(data);
}

/* <channel> *( "," <channel> ) <user> *( "," <user> ) */
void irc_callback_kick(irc_connection *ic, char *command, GString **p, int n)
{
    char *from;
    channel *c;
    
    if(n < 4)
    {
        LOG(LOG_ERROR, "%d < 4 failed\n", n);
        
        return;
    }

    from = irc_prefix_nickname(p[0]->str);

    LOG(LOG_DEBUG, "channel=%s from=%s kicked=%s\n", p[2]->str, from, p[3]->str);
    
    LOCK(data);
    
    if(strcmp(p[3]->str, selfname->str) == 0) /* we was kicked */
    {
        if(channel_del(p[2]->str) == TRUE)
            LOG(LOG_DEBUG, "self kick removed channel\n");
        else
            LOG(LOG_ERROR, "self kick unknown channel\n");
    }
    else /* someone else was kicked */
    {
        c = channel_find(p[2]->str);
        
        if(c == NULL)
            LOG(LOG_DEBUG, "kick unknown channel\n");
        else
        {
            if(user_del(c, p[3]->str) == TRUE)
                LOG(LOG_DEBUG, "kick ok\n");
            else
                LOG(LOG_ERROR, "kick unknown user\n");
        }
    }
    
    UNLOCK(data);
    
    /* if KICK was from us a thread might be waiting */
    if(strcmp(from, selfname->str) == 0)
        TALK_PUSH("KICK");
}

