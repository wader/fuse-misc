/*
 * irctest, simple client with remote control
 * mattias@sudac.org
 *
 * Example usage:
 * ./irctest -d -v localhost 6667 "" irctest foo bar bla
 * ./irctest -d -v localhost 6667 "" irctest foo bar ""
 *
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

#include "irc.h"

char *remote_password;
int verbose;
int debug;


void irc_callback_debug(irc_connection *ic, char *command, GString **p, int n)
{
    int i;

    fprintf(stderr, "DEBUG:");
    for(i = 0; i < n; i++)
        fprintf(stderr, " %d=\"%s\"(%d)", i, p[i]->str, p[i]->len);
    fprintf(stderr, "\n");
}

void irc_callback_ping(irc_connection *ic, char *command, GString **p, int n)
{
    if(n > 2)
        irc_send(ic, "PONG %s", p[2]->str);
}

void irc_callback_privmsg(irc_connection *ic, char *command, GString **p, int n)
{
    char *s;
    
    if(n < 4)
        return;

    /* ctcp */
    if(p[3]->str[0] == '\001' && p[3]->str[p[3]->len - 1] == '\001')
    {
        if(strncmp(p[3]->str + 1, "PING", 4) == 0)
            irc_send(ic, "NOTICE %s :%s", irc_prefix_nickname(p[0]->str), p[3]->str); /* im lazy, just echo */
        else if(strncmp(p[3]->str + 1, "VERSION", 7) == 0)
            irc_send(ic, "NOTICE %s :\001VERSION irctest", irc_prefix_nickname(p[0]->str));
        
        return;
    }

    if(strcmp(remote_password, "") == 0)
        s = p[3]->str;
    else
    {
        s = strchr(p[3]->str, ' ');
        if(s == NULL)
            return;

        *s++ = '\0'; /* terminate password string and make s point to rest of message */
        
        if(strcmp(p[3]->str, remote_password) != 0)
        {
            if(verbose)
                fprintf(stdout, "Wrong password \"%s\" from %s: \"%s\"\n", p[3]->str, irc_prefix_nickname(p[0]->str), s);
                        
            return;
        }
    }

    if(verbose)
        fprintf(stdout, "Remote control from %s: \"%s\"\n", irc_prefix_nickname(p[0]->str), s);
    
    irc_send(ic, "%s", s);
}


int main(int argc, char **argv)
{
    irc_connection *c;
    
    debug = 0;
    verbose = 0;

    for(; argc > 1 && argv[1][0] == '-'; argv++, argc--)
    {
        if(strcmp(argv[1], "-d") == 0)
            debug = 1;
        else if(strcmp(argv[1], "-v") == 0)
            verbose = 1;
        else
        {
            fprintf(stderr, "Unknown option %s\n", argv[1]);

            return EXIT_FAILURE;
        }
    }
   
    if(argc < 8)
    {
        fprintf(stderr, "Usage: %s [-v] [-d] server port password nickname username realname remotepasssword\n", argv[0]);
        
        return EXIT_FAILURE;
    }

    remote_password = argv[7];
    
    c = irc_alloc();

    if(debug)
        irc_define_callback(c, NULL, irc_callback_debug);
    irc_define_callback(c, "PING", irc_callback_ping);
    irc_define_callback(c, "PRIVMSG", irc_callback_privmsg);
    
    if(irc_connect(c, argv[1], atoi(argv[2]), argv[3], argv[4], argv[5], argv[6]) != -1)
    {
        irc_loop(c);
        irc_disconnect(c);
    }
    
    irc_free(c);
    
    return EXIT_SUCCESS;
}

