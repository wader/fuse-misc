/*
 * ircfs
 *
 * mattias@sudac.org
 */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <glib.h>
#include <fuse.h>

#include "ircfs.h"
#include "irc.h"
#include "irc_data.h"


static char *path_split_dup(const char *path, char **rest)
{
    char *s;

    s = g_strdup((char *)path + (*path == '/' ? 1 : 0)); /* skip first / if its there */
    *rest = strchr(s, '/');
    if(*rest != NULL)
        *(*rest)++ = '\0'; /* terminate first and make rest pointer to rest of path */

    return s;
}

static int ircfs_getattr_channel_users(channel *c, char *path, struct stat *st)
{
    int result = 0;
    user *u = NULL;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    if(path == NULL) /* /channel/users */
    {
        st->st_mode = S_IFDIR | 0755;
        st->st_nlink = 2 + g_list_length(c->users);
        st->st_atime = st->st_mtime = st->st_ctime = c->mtime;
        result = 0;
    }
    else /* /channel/users/x */
    {
        u = user_find(c, path + (irc_is_mode(*path) ? 1 : 0));

        if(u == NULL)
            result = -ENOENT;
        else
        {
            st->st_mode = S_IFREG | 0644;
            st->st_nlink = 1;
            st->st_atime = st->st_mtime = st->st_ctime = u->mtime;
            result = 0;
        }
    }

    return result;
}

static int ircfs_getattr_channel(channel *c, char *path, struct stat *st)
{
    char *n = NULL, *r = NULL;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    if(path == NULL) /* /channel */
    {
        st->st_mode = S_IFDIR | 0755;
        st->st_nlink = 2 + 1; /* 2 + topic */
        st->st_atime = st->st_mtime = st->st_ctime = c->mtime;
        result = 0;
    }
    else  /* /channel/x */
    {
        n = path_split_dup(path, &r);
    
        LOG(LOG_DEBUG, "n=%s r=%s\n", n, r);

        if(strcmp(n, "topic") == 0)
        {
            st->st_mode = S_IFREG | 0644;
            st->st_nlink = 1;
            st->st_size = c->topic->len;
            result = 0;
        }
        else if(strcmp(n, "users") == 0)
            result = ircfs_getattr_channel_users(c, r, st);
        else
            result = -ENOENT;
        
        g_free(n);
    }
    
    return result;
}

int ircfs_getattr(const char *path, struct stat *st)
{
    char *n, *r;
    channel *c;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    memset(st, 0, sizeof(struct stat));

    st->st_uid = getuid();
    st->st_gid = getgid();
   
    n = path_split_dup(path, &r);

    LOCK(data);
    
    if(strcmp(n, "") == 0) /* / */
    {
        st->st_mode = S_IFDIR | 0755;
        st->st_nlink = 2 + g_list_length(channels);
        result = 0;
    }
    else if(strcmp(n, "quote") == 0)
    {
        st->st_mode = S_IFREG | 0222;
        st->st_nlink = 1;
        st->st_size = 0;
        result = 0;
    }
    else if(strcmp(n, "motd") == 0)
    {
        st->st_mode = S_IFREG | 0444;
        st->st_nlink = 1;
        st->st_size = motd->len;
        result = 0;
    }
    else if((c = channel_find(n)) != NULL)
        result = ircfs_getattr_channel(c, r, st);
    else
        result = -ENOENT;
    
    UNLOCK(data);

    g_free(n);
    
    return result;
}

int ircfs_getdir_channel_users(channel *c, char *path, fuse_dirh_t h, fuse_dirfil_t filler)
{
    int result = 0;
    GList *l = NULL;
    GString *filename = NULL;
    user *u = NULL;
    
    LOG(LOG_DEBUG, "%s\n", path);

    if(path == NULL) /* /channel/users */
    {
        filename = g_string_new("");
        
        for(l = c->users; l; l = l->next)
        {
            u = l->data;

            /* NOTE: someone with op and voice is shown as a op, like other IRC clients */
            if(u->op == TRUE)
                filename = g_string_assign(filename, "@");
            else if(u->halfop == TRUE)
                filename = g_string_assign(filename, "%");
            else if(u->voice == TRUE)
                filename = g_string_assign(filename, "+");
            else
                filename = g_string_assign(filename, "");
            filename = g_string_append(filename, u->name->str);
            
            /* filler (lib/fuse.c:fill_dir) seams not to need string after call */
            LOG(LOG_DEBUG, "filler: %s\n", filename->str);
            filler(h, filename->str, DT_REG);
        }
    
        g_string_free(filename, TRUE);
    }
    else /* /channel/users/x */
        result = -ENOENT;

    return result;
}

int ircfs_getdir_channel(channel *c, char *path, fuse_dirh_t h, fuse_dirfil_t filler)
{
    char *n = NULL, *r = NULL;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    if(path == NULL) /* /channel */
    {
        filler(h, "users", DT_DIR);
        filler(h, "topic", DT_REG);
        
        result = 0;
    }
    else /* /channel/x */
    {
        n = path_split_dup(path, &r);

        if(strcmp(n, "users") == 0)
            result = ircfs_getdir_channel_users(c, r, h, filler);
        else
            result = -ENOENT;

        g_free(n);
    }

    return result;
}

int ircfs_getdir(const char *path, fuse_dirh_t h, fuse_dirfil_t filler)
{
    char *n, *r;
    channel *c;
    int result;
    GList *l;
    
    LOG(LOG_DEBUG, "%s\n", path);
   
    n = path_split_dup(path, &r);
    
    LOCK(data);
    
    if(strcmp(n, "") == 0)
    {
        filler(h, "quote", DT_REG);
        
        if(motd != NULL)
            filler(h, "motd", DT_REG);
    
        for(l = channels; l; l = l->next)
        {
            c = l->data;
            LOG(LOG_DEBUG, "filler: %s\n", c->name->str);
            filler(h, c->name->str, DT_DIR);
        }
    
        result = 0;
    }
    else if((c = channel_find(n)) != NULL)
        result = ircfs_getdir_channel(c, r, h, filler);
    else
        result = -ENOENT;
    
    UNLOCK(data);

    if(result == 0)
    {
        filler(h, ".", DT_DIR);
        filler(h, "..", DT_DIR);
    }

    g_free(n);
    
    return result;
}

int ircfs_mkdir(const char *path, mode_t mode)
{
    char *n, *r, *status;
    int result = 0;
    channel *c;
    
    LOG(LOG_DEBUG, "%s\n", path);
   
    n = path_split_dup(path, &r);
 
    if(r == NULL) /* /x */
    {
        LOCK(talk);
        LOCK(data);
        c = channel_find(n);
        UNLOCK(data);
        
        if(c == NULL)
        {
            talk_wait = TRUE;
            irc_send(connection, "JOIN %s", n);
            TALK_POP(status);
            
            if(strcmp(status, "JOIN") == 0)
                result = 0;
            else if(status == ERR_NOSUCHCHANNEL)
                result = -EINVAL;
            else if(status == ERR_BANNEDFROMCHAN ||
                    status == ERR_INVITEONLYCHAN ||
                    status == ERR_BADCHANNELKEY)
                result = -EACCES;
            else if(status == ERR_CHANNELISFULL)
                result = -ENOSPC; /* no space left */
            else
                result = -EPERM;
        }
        else
            result = -EEXIST;
        
        UNLOCK(talk);
    }
    else /* /x/y... */
        result = -EPERM;

    g_free(n);

    return result;
}

int ircfs_rmdir(const char *path)
{
    char *n, *r, *status;
    int result = 0;
    channel *c;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    n = path_split_dup(path, &r);
    
    if(r == NULL) /* /x */
    {
        LOCK(talk);
        LOCK(data);
        c = channel_find(n);
        UNLOCK(data);

        if(c != NULL)
        {
            talk_wait = TRUE;
            irc_send(connection, "PART %s :rmdir", n);
            TALK_POP(status);

            if(strcmp(status, "PART") == 0)
                result = 0;
            else if(status == ERR_NOTONCHANNEL)
                result = -ENOENT;
            else
                result = -EINVAL; /* NOSUCHCHANNEL etc */
        }
        else
            result = -ENOENT;
        
        UNLOCK(talk);
    }
    else /* /x/y... */
        result = -ENOENT;

    g_free(n);

    return result;
}

/* TODO: maybe use some field for number of channels etc? */
/* path? */
int ircfs_statfs(const char *path, struct statfs *st)
{
    return
        st->f_bsize =
        st->f_blocks =
        st->f_bfree =
        st->f_files =
        st->f_ffree =
        st->f_namelen = 0; /* more? bits/statfs.h */
}

int ircfs_open(const char *path, int flags)
{
    char *n, *r;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    n = path_split_dup(path, &r);
    
    LOCK(data);
    
    if(strcmp(n, "motd") == 0)
    {
        if((flags & 3) == O_RDONLY)
            result = 0;
        else
            result = -EACCES;
    }

    UNLOCK(data);

    g_free(n);

    return result;
}

static int ircfs_read_channel(channel *c, char *path, char *buf, size_t size, off_t offset)
{
    char *n = NULL, *r = NULL;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    if(path == NULL)
        result = -EISDIR;
    else
    {
        n = path_split_dup(path, &r);

        if(strcmp(n, "topic") == 0)
        {
            /* TODO: security? */
            memcpy(buf, c->topic->str + offset, size);
            result = size;
        }
        else if(strcmp(n, "users") == 0)
            result = -EISDIR;
        else
            result = -ENOENT;

        g_free(n);
    }

    return result;
}
    
int ircfs_read(const char *path, char *buf, size_t size, off_t offset)
{
    char *n = NULL, *r = NULL;
    int result = 0;
    channel *c = NULL;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    n = path_split_dup(path, &r);

    LOCK(data);
    
    if(strcmp(n, "motd") == 0)
    {
        /* TODO: security? */
        memcpy(buf, motd->str + offset, size);
        result = size;
    }
    else if(strcmp(n, "quote") == 0)
        result = -EACCES;
    else if((c = channel_find(n)) != NULL)
        result = ircfs_read_channel(c, r, buf, size, offset);
    else
        result = -ENOENT;
    
    UNLOCK(data);

    g_free(n);

    return result;
}

static int ircfs_write_channel(char *name, char *path, const char *buf, size_t size, off_t offset)
{
    char *r = NULL, *n = NULL;
    int result = 0;
    char *status = NULL;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    if(path == NULL) /* channel */
        result = -EISDIR;
    else /* /channel/x */
    {
        n = path_split_dup(path, &r);
        
        if(r == NULL) /* /channel/x */
        {
            if(strcmp(n, "topic") == 0)
            {
                char *s;
                
                /* HACK */
                s = g_strndup(buf, size + 1);
                s[size] = '\0';
                
                LOCK(talk);
                talk_wait = TRUE;
                irc_send(connection, "TOPIC %s :%s", name, s);
                TALK_POP(status);

                if(strcmp(status, "TOPIC") == 0)
                    result = size;
                else if(status == ERR_CHANOPRIVSNEEDED)
                    result = -EACCES;
                else if(status == ERR_NOTONCHANNEL)
                    result = -ENOENT;
                else
                    result = -EPERM;
                
                UNLOCK(talk);
                
                g_free(s);
            }
            else if(strcmp(n, "users") == 0)
                result = -EISDIR;
            else
                result = -ENOENT;
        }
        else /* /channel/x/.. */
        {
        }

        g_free(n);
    }

    return result;
}
    
/* TODO: buffer and wait for \n? */
int ircfs_write(const char *path, const char *buf, size_t size, off_t offset)
{
    char *r = NULL, *n = NULL;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    n = path_split_dup(path, &r);
    
    if(strcmp(n, "") == 0)
       result = -EISDIR;
    else if(strcmp(n, "motd") == 0)
       result = -EACCES;
    else if(strcmp(n, "quote") == 0)
    {
        char *s;
        
        /* HACK */
        s = g_strndup(buf, size + 1);
        s[size] = '\0';
        
        LOCK(talk);
        irc_send(connection, "%s", s);
        UNLOCK(talk);
        
        g_free(s);

        result = size;
    }
    else
        result = ircfs_write_channel(n, r, buf, size, offset);

    g_free(n);

    return result;
}

#ifdef BLA
static int ircfs_unlink_channel_users(char *channel, char *name, char *path)
{

    int result = 0;
    channel *c = NULL;
    user *u = NULL;
    char *status;

    LOCK(data);
    c = channel_find(channel);
    if(c != NULL)
        u = user_find(c, path + (irc_is_mode(*path) ? 1 : 0));
    UNLOCK(data);

    if(c == NULL || u == NULL)
        return -ENOENT;

    talk_wait = TRUE;
    irc_send(connection, "KICK %s %s", name, path + (irc_is_mode(*path) ? 1 : 0));
    TALK_POP(status);

    if(strcmp(status, "KICK") == 0)
        result = 0;
    else if(status == ERR_CHANOPRIVSNEEDED)
        result = -EACCES;
    else if(status == ERR_USERNOTINCHANNEL ||
            status == ERR_NOSUCHCHANNEL ||
            status == ERR_NOTONCHANNEL)
        result = -ENOENT;
    else
        result = -EPERM;            
    
    return result;
}

static int ircfs_unlink_channel(char *channel, char *path)
{
    char *r, *n;
    int result = 0;
    channel *c;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    n = path_split_dup(path, &r);
    
    LOCK(data);
    c = channel_find(channel);
    if(c != NULL)
        u = user_find(c, path + (irc_is_mode(*path) ? 1 : 0));
    UNLOCK(data);

    if(path == NULL)
        result = -EISDIR;
    else
    {
        n = path_split_dup(path, &r);

        if(r == NULL) /* /channel/x */
        {
        }
        else /* /channel/x/... */
        {
        }
            
    }
    
    if(r == NULL) /* /x/ */
    {
        LOCK(data);
        
        if(strcmp(n, "") == 0 ||
           channel_find(n) != NULL)
            result = -EISDIR;
        else if(strcmp(n, "motd") ||
                strcmp(n, "quote"))
            result = -EACCES; /* -EPERM? */
        else
            result = -ENOENT;
        
        UNLOCK(data);
    }
    else /* /x/y... */
        result = ircfs_unlink_channel(n, r);
    
    g_free(n);

    return result;
}
#endif

int ircfs_unlink(const char *path)
{
    char *r, *n;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);
    
    n = path_split_dup(path, &r);

    if(r == NULL) /* /x */
    {
        LOCK(data);
        
        if(strcmp(n, "") == 0 ||
           channel_find(n) != NULL)
            result = -EISDIR;
        else if(strcmp(n, "motd") ||
                strcmp(n, "quote"))
            result = -EACCES; /* -EPERM? */
        else
            result = -ENOENT;
        
        UNLOCK(data);
    }
//    else /* /x/y... */
//        result = ircfs_unlink_channel(n, r);
    
    g_free(n);

    return result;
}

static int ircfs_truncate_channel(channel *c, char *path, off_t size)
{
    char *r = NULL, *n = NULL;
    int result = 0;
    
    LOG(LOG_DEBUG, "%s\n", path);

    if(path == NULL)
        result = -EISDIR;
    else
    {
        n = path_split_dup(path, &r);
        
        if(strcmp(n, "topic") == 0)
            result = 0; /* something more? */
        else if(strcmp(n, "users") == 0)
            result = -EISDIR;
        else
            result = -ENOENT;

        g_free(n);
    }
    
    return result;
}

int ircfs_truncate(const char *path, off_t size)
{
    char *r = NULL, *n = NULL;
    int result = 0;
    channel *c = NULL;
    
    LOG(LOG_DEBUG, "%s\n", path);

    n = path_split_dup(path, &r);

    LOCK(data);

    if(strcmp(n, "quote") == 0)
        result = 0;
    else if(strcmp(n, "") == 0)
        result = -EISDIR;
    else if(strcmp(n, "motd") == 0)
        result = -EACCES;
    else if((c = channel_find(n)) != NULL)
        result = ircfs_truncate_channel(c, r, size);
    else
        result = -ENOENT;

    UNLOCK(data);
    
    g_free(n);

    return result;
}


