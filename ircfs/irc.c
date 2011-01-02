/*
 * ircfs
 *
 * how to handle errors? silent drop..?
 * use char/gchar insted of GString for parameter list?
 * better event loop handling, loop quit func?
 *
 * mattias@sudac.org
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdarg.h>
#include <glib.h>

#include "irc.h"
#ifndef IRCTEST
#include "ircfs.h"
#endif

static const char irc_message_separator[] = {'\r', '\n'}; /* CR LF */


int irc_is_mode(char c)
{
    switch(c)
    {
        case '@': return TRUE; break;
        case '%': return TRUE; break;
        case '+': return TRUE; break;
        default: return FALSE; break;
    }
}

char *irc_prefix_nickname(char *s)
{
    char *p = NULL;
    
    p = strchr(s, '!');
    if(p == NULL)
        p = strchr(s, '@');
    
    if(p != NULL)
        *p = '\0';

    return s;
}

static void irc_parse(irc_connection *connection, GString *message)
{
    GString **parameters = NULL;
    char *walk, *next = NULL, *key = NULL;
    irc_callback callback = NULL;
    int n, i;

    n = 0;
    g_string_truncate(message, message->len - sizeof(irc_message_separator));

#ifndef IRCTEST
    LOG(LOG_NETWORK, "\"%s\"\n", message->str);
#endif

    /* first parameter is not prefix, fake empty prefix */
    if(message->str[0] != ':')
    {
        parameters = g_renew(GString*, parameters, 1);
        parameters[0] = g_string_new("");
        n++;
    }

    for(walk = next = message->str; next != NULL; walk = next + 1)
    {
        next = strchr(walk, ' ');

        if(walk[0] == ':')
        {
            walk++; /* skip colon */

            /* not first parameter, use rest as parameter */
            if(n > 0)
                next = NULL;
        }
        
        parameters = g_renew(GString*, parameters, n + 1);
        
        if(next == NULL)
            parameters[n] = g_string_new(walk);
        else
            parameters[n] = g_string_new_len(walk, next - walk);

        n++;
    }

    if(n > 1)
    {
        if(connection->default_callback != NULL)
            connection->default_callback(connection, NULL, parameters, n);

        if(g_hash_table_lookup_extended(
                    connection->callbacks,
                    parameters[1]->str,
                    (gpointer)&key,
                    (gpointer)&callback) == TRUE)
            callback(connection, key, parameters, n);
    }
    else
        fprintf(stderr, "irc_parse: %d > 1 failed: message=\"%s\"", n, message->str);

    for(i = 0; i < n; i++)
        g_string_free(parameters[i], TRUE);
    g_free(parameters);
}

irc_connection *irc_alloc()
{
    irc_connection *c;

    c = g_new(irc_connection, 1);
    
    c->callbacks = g_hash_table_new((GHashFunc)g_str_hash, (GEqualFunc)g_str_equal);
    c->default_callback = NULL;
    c->io_channel = NULL;

    return c;
}

void irc_free(irc_connection *connection)
{
    g_hash_table_destroy(connection->callbacks);
    g_free(connection);
}

void irc_define_callback(irc_connection *connection, char *command, irc_callback function)
{
    if(command == NULL)
        connection->default_callback = function;
    else
        g_hash_table_insert(connection->callbacks, command, function);
}

int irc_send(irc_connection *connection, char *format, ...)
{
    gchar *s;
    GString *buffer = NULL;
    va_list args;
    GIOStatus status;
    gsize offset, bytes_written;

    if(connection->io_channel == NULL)
        return -1;
    
    va_start(args, format);
    s = g_strdup_vprintf(format, args);
    va_end(args);
    buffer = g_string_new(s);
    g_free(s);
/* make irctest.c happy */
#ifndef IRCTEST
    LOG(LOG_NETWORK, "\"%s\"\n", buffer->str);
#endif
    g_string_append_len(buffer, irc_message_separator, sizeof(irc_message_separator));
    
    offset = 0;

    while(TRUE)
    {
        status = g_io_channel_write_chars(
                connection->io_channel,
                buffer->str + offset,
                buffer->len - offset,
                &bytes_written,
                NULL
                );
        if(status == G_IO_STATUS_NORMAL)
        {
            g_io_channel_flush(connection->io_channel, NULL);
            
            break;
        }
        else if(status == G_IO_STATUS_AGAIN)
        {
            offset += bytes_written;

            continue;
        }
        else /* eof or error */
        {
            irc_disconnect(connection);

            break;
        }
    }
    
    g_string_free(buffer, TRUE);

    return (status == G_IO_STATUS_NORMAL ? 0 : -1);
}

int irc_connect(irc_connection *connection, char *hostname, int port, char *password, char *nickname, char *username, char *realname)
{
    struct sockaddr_in remote;
    struct hostent *h = NULL;
    int socket_fd;
    int status;

    socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    if(socket_fd == -1)
    {
        perror("socket");
        
        return -1;
    }

    h = gethostbyname(hostname);
    if(h == NULL)
    {
        herror("gethostbyname");

        return -1;
    }

    remote.sin_family = AF_INET;
    memcpy(&remote.sin_addr.s_addr, h->h_addr_list[0], h->h_length);
    remote.sin_port = htons(port);
    if(connect(socket_fd, (struct sockaddr*)&remote, sizeof(remote)) == -1)
    {
        perror("connect");

        return -1;
    }

    connection->io_channel = g_io_channel_unix_new(socket_fd);
    g_io_channel_set_line_term(connection->io_channel, irc_message_separator, sizeof(irc_message_separator));
    g_io_channel_set_encoding(connection->io_channel, NULL, NULL); /* raw */

    status = 0;
    /* if password is not "" */
    if(strcmp(password, "") != 0)
        status = irc_send(connection, "PASS %s", password);
    if(status == 0)
        status = irc_send(connection, "NICK %s", nickname);
    if(status == 0)
        /* 0 is modes (none), * is an unused parameter in the protocol */
        status = irc_send(connection, "USER %s 0 * :%s", username, realname);
   
    if(status == -1)
    {
        irc_disconnect(connection);

        return -1;
    }
    
    return 0;
}

void irc_disconnect(irc_connection *connection)
{
    if(connection->io_channel == NULL)
        return;
    
    g_io_channel_shutdown(connection->io_channel, TRUE, NULL);
    g_io_channel_unref(connection->io_channel);
    connection->io_channel = NULL;
}

void irc_loop(irc_connection *connection)
{
    GIOStatus status;
    GString *buffer = NULL;

    buffer = g_string_new("");
    
    while(TRUE)
    {
        status = g_io_channel_read_line_string(connection->io_channel, buffer, NULL, NULL);
        if(status == G_IO_STATUS_NORMAL)
            irc_parse(connection, buffer);
        else if(status == G_IO_STATUS_AGAIN)
            continue;
        else /* error of eof */
        {
            irc_disconnect(connection);
            
            break;
        }
    }
    
    g_string_free(buffer, TRUE);
}

