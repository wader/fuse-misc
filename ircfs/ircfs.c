/*
 * ircfs
 * 
 * mattias@sudac.org
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <getopt.h>
#include <stdarg.h>
#include <signal.h>
#include <fuse.h>

#include "ircfs.h"
#include "irc.h"
#include "irc_data.h"
#include "irc_callbacks.h"
#include "fuse_callbacks.h"


G_LOCK_DEFINE(talk);

static char *server;
static char *port;
static char *password;
static char *nickname;
static char *username;
static char *realname;

static struct fuse *fuse;
static int fuse_fd;
static char *fuse_mountpoint;

int bla;

static struct fuse_operations ircfs_operations =
{
    .getattr    = ircfs_getattr,
    .getdir     = ircfs_getdir,
    .mkdir      = ircfs_mkdir,
    .rmdir      = ircfs_rmdir,
    .statfs     = ircfs_statfs,
    .open       = ircfs_open,
    .read       = ircfs_read,
    .write      = ircfs_write,
    .unlink     = ircfs_unlink,
    .truncate   = ircfs_truncate,
};


gpointer irc_thread(gpointer data)
{
    LOG(LOG_DEBUG, "start\n");
    
    connection = irc_alloc();

    /* TODO: if debug */
    irc_define_callback(connection, NULL, irc_callback_debug);

    /* TODO: document errors */
    irc_define_callback(connection, ERR_NICKNAMEINUSE, irc_callback_talk_push);
    irc_define_callback(connection, ERR_NICKCOLLISION, irc_callback_talk_push);
    irc_define_callback(connection, ERR_BANNEDFROMCHAN, irc_callback_talk_push);
    irc_define_callback(connection, ERR_INVITEONLYCHAN, irc_callback_talk_push);
    irc_define_callback(connection, ERR_BADCHANNELKEY, irc_callback_talk_push);
    irc_define_callback(connection, ERR_CHANNELISFULL, irc_callback_talk_push);
    irc_define_callback(connection, ERR_BADCHANMASK, irc_callback_talk_push);
    irc_define_callback(connection, ERR_NOSUCHCHANNEL, irc_callback_talk_push); /* join, kick */
    irc_define_callback(connection, ERR_TOOMANYCHANNELS, irc_callback_talk_push);
    irc_define_callback(connection, ERR_TOOMANYTARGETS, irc_callback_talk_push);
    irc_define_callback(connection, ERR_CHANOPRIVSNEEDED, irc_callback_talk_push); /* kick */
    irc_define_callback(connection, ERR_USERNOTINCHANNEL, irc_callback_talk_push); /* kick */
    irc_define_callback(connection, ERR_NOTONCHANNEL, irc_callback_talk_push); /* kick */
    irc_define_callback(connection, ERR_NOCHANMODES_RFC2812, irc_callback_talk_push); /* topic */ /* ?? */

    
    irc_define_callback(connection, ERR_UNAVAILRESOURCE_RFC2812, irc_callback_talk_push);

    irc_define_callback(connection, RPL_WELCOME, irc_callback_talk_push);
    irc_define_callback(connection, RPL_MOTDSTART, irc_callback_motd_start);
    irc_define_callback(connection, RPL_MOTD, irc_callback_motd);
    irc_define_callback(connection, RPL_NAMREPLY, irc_callback_names_reply);
    /*irc_define_callback(connection, RPL_NOTOPIC, irc_callback_topic_reply);*/ /* ?? */
    irc_define_callback(connection, RPL_TOPIC, irc_callback_topic_reply);
    
    irc_define_callback(connection, "PING", irc_callback_ping);
    irc_define_callback(connection, "JOIN", irc_callback_join);
    irc_define_callback(connection, "PART", irc_callback_part);
    irc_define_callback(connection, "MODE", irc_callback_mode);
    irc_define_callback(connection, "KICK", irc_callback_kick);
    irc_define_callback(connection, "TOPIC", irc_callback_topic);

    if(irc_connect(connection, server, atoi(port), password, selfname->str, username, realname) != -1)
    {
        irc_loop(connection);
        irc_disconnect(connection);
    }
    else
        TALK_PUSH("ERROR:Connect");

    /* TODO: if we was killed by server this thread will hang until read in fuse loop wakes up */
    /* send dummy signal or flose fuse->fd somehow? */
    //if(fuse != NULL)
    //    fuse_exit(fuse);
    kill(bla, SIGINT);
    //
    
    
    irc_free(connection);
    
    LOG(LOG_DEBUG, "stop\n");
    
    return NULL;
}

static void help(char *program)
{
    fprintf(stderr,
            "Usage: %s [OPTION]... MOUNTPOINT SERVER NICKNAME\n"
            "OPTIONS:\n"
            "    -p PASSWORD  Server password (it not set, no password is sent)\n"
            "    -n PORT      Server port (%s)\n"
            "    -u NAME      Set username (%s)\n"
            "    -r NAME      Set real name (%s)\n"
            "    -d LEVEL     Set debug level\n"
            "    -f           Enable fuse debug\n"
            "",
            program, port, username, realname
            );
}

static void fuse_quit()
{
    if(fuse != NULL)
        fuse_destroy(fuse);
    if(fuse_fd != -1)
        fuse_unmount(fuse_mountpoint);
}

static void die(char *format, ...)
{
    va_list args;
    
    fprintf(stderr, "Exit: ");
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fuse_quit();
    
    exit(EXIT_FAILURE);
}

static void signal_handler(int signal)
{
    fprintf(stderr, "Got signal\n");
    
    if(fuse != NULL)
        fuse_exit(fuse);
}

int main(int argc, char **argv)
{
    int i;
    GThread *t;
    char *s;
    char *working_dir;
    char *fusemount_args[] =
    {
        "-n", "ircfs", /* filesystem name */
        NULL
    };
    int fuse_flags = 0;
    int getopt_c;
   
    bla = getpid();
    
    /* HACK: log everything for now */
    for(i = 0; i < ARRAY_ELEMENTS(log_types); i++)
        log_types[i] = 1; /* TODO: 0 */
    
    /* init and set default values */
    if(!g_thread_supported())
        g_thread_init(NULL);
    working_dir = get_current_dir_name();
    if(working_dir == NULL)
        die("get_current_dir_name failed for working_dir\n");
    fuse = NULL;
    fuse_fd = -1;
    talk_status = g_async_queue_new();
    irc_data_init();
    server = NULL;
    port = "6667";
    password = ""; /* empty passwords are not allowed in the RFC anyway */
    nickname = NULL;
    username = "ircfs";
    realname = "ircfs";

    for(;;)
    {
        getopt_c = getopt(argc, argv, "hp:n:u:r:d:f");

        if(getopt_c == -1)
            break;
        else if(getopt_c == 'p')
            password = optarg;
        else if(getopt_c == 'n')
            port = optarg;
        else if(getopt_c == 'u')
            username = optarg;
        else if(getopt_c == 'r')
            realname = optarg;
        else if(getopt_c == 'd')
        {
            /* TODO: */
        }
        else if(getopt_c == 'f')
            fuse_flags |= FUSE_DEBUG;
        else if(getopt_c == 'h')
            help(argv[0]);
        else
            exit(EXIT_FAILURE);
    }

    if(optind != argc - 3)
    {
        help(argv[0]);

        exit(EXIT_FAILURE);
    }

    if(argv[optind][0] == '/')
        fuse_mountpoint = argv[optind];
    else
    {
        asprintf(&fuse_mountpoint, "%s%s%s",
                 working_dir,
                 /* avoid doubel slash, get_c... can end with a slash, see man page about $PWD */
                 (working_dir[strlen(working_dir) - 1] == '/' ? "" : "/"),
                 argv[optind]
                 );
        if(fuse_mountpoint == NULL)
            die("asprintf fuse_mountpoint failed\n");
    }
    server = argv[optind + 1];
    nickname = argv[optind + 2];
    
    selfname = g_string_new(nickname);
  
    /* start irc thread and wait for reply */
    talk_wait = TRUE;
    t = g_thread_create(irc_thread, NULL, TRUE, NULL);
    if(t == NULL)
        die("g_thread_create failed for irc thread\n");
    TALK_POP(s);
  
    signal(SIGINT, signal_handler);
    signal(SIGHUP, signal_handler);
    signal(SIGTERM, signal_handler);
    
    if(s == RPL_WELCOME)
    {
        LOG(LOG_DEBUG, "fuse start\n");
        
        fuse_fd = fuse_mount(fuse_mountpoint, (const char **)fusemount_args);
        if(fuse_fd == -1)
            die("fuse_mount failed\n");
        fuse = fuse_new(fuse_fd, fuse_flags, &ircfs_operations);
        if(fuse == NULL)
            die("fuse_new failed\n");
        fuse_loop_mt(fuse);
        
        LOG(LOG_DEBUG, "fuse stop\n");

        /* TODO: not thread safe? LOCK(talk) ? */
        if(connection->io_channel != NULL)
            irc_send(connection, "QUIT :unmount");
    }
    else if(strncmp(s, "ERROR:", 6) == 0)
    {
        fprintf(stderr, "Error: %s\n", s);
    }
    else
    {
        irc_send(connection, "QUIT");
        
        if(s == ERR_NICKNAMEINUSE)
            fprintf(stderr, "Error: Nickname is already in use\n");
        else
            fprintf(stderr, "Error: %s\n", s);
    }

    LOG(LOG_DEBUG, "joining irc thread\n");
    g_thread_join(t);

    irc_data_free();
    
    LOG(LOG_DEBUG, "done\n");

    fuse_quit();

    return EXIT_SUCCESS;
}

