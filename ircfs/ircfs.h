#ifndef __IRCFS_H__
#define __IRCFS_H__

#include "irc.h"

int log_types[256];
irc_connection *connection;
GAsyncQueue *talk_status;
gboolean talk_wait;
G_LOCK_EXTERN(talk);

#define ARRAY_ELEMENTS(array) (sizeof(array)/sizeof(array[0]))

#define LOG_ERROR 'e'
#define LOG_DEBUG 'd'
#define LOG_NETWORK 'n'

/* FIXME: add debug option */
#define LOG(type, format, args...) \
    do \
    { \
        if(log_types[type] != 0) \
            fprintf(stderr, "LOG %c: %s: " format, type, __func__, ##args); \
    } while(0)

#define LOCK(l) \
    do \
    { \
        LOG(LOG_DEBUG, "locking " #l "\n"); \
        G_LOCK(l); \
        LOG(LOG_DEBUG, "got lock " #l "\n"); \
    } while(0)

#define UNLOCK(l) \
    do \
    { \
        LOG(LOG_DEBUG, "unlocking " #l "\n"); \
        G_UNLOCK(l); \
    } while(0)

#define TALK_POP(s) \
    do \
    { \
        LOG(LOG_DEBUG, "waiting for status\n"); \
        s = g_async_queue_pop(talk_status); \
        talk_wait = FALSE; \
        LOG(LOG_DEBUG, "got status \"%s\"\n", s); \
    } while(0)

#define TALK_PUSH(s) \
    do \
    { \
        if(talk_wait == TRUE) \
        { \
            talk_wait = FALSE; \
            LOG(LOG_DEBUG, "sending status \"%s\"\n", s); \
            g_async_queue_push(talk_status, s); \
            LOG(LOG_DEBUG, "message sent\n"); \
        } \
        else \
            LOG(LOG_DEBUG, "not sending \"%s\", no one is waiting\n", s); \
    } while(0)

#endif

