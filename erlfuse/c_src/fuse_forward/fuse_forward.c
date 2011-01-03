/*
 * fuse_forward, communicate with fuse using stdin/stdout.
 * Uses libfuse for doing argument parsing and mounting to get fd to fuse
 * device.
 *
 * Protocol:
 *
 * Uses the {packet, 4} option for port_open.
 * All messages are preceded with their length, 32 bit network order.
 *
 * stdin:
 * raw fuse messages to be sent to fuse
 * 
 * stdout:
 * term in erlang binary format
 *
 * {read, Data}
 *      Message from fuse, can be partial, can be many
 *      Data is a binary
 * {write, Errno}
 *      Errno of whole fuse message write
 *      Errno is an integer
 * {error, Error}
 *      Error message
 *      Error is a string
 *
 * There will be one {write, Errno} message per whole fuse message read from
 * stdin, it is used to check write errno, mostly ENOENT, message was
 * interrupted, and EINVAL, invalid message.
 *
 */

/* NOTES:
 * comment out stderr redirect stuff before debugging or you will have a loop.
 *
 * TODO:
 * max message size sanity
 * errors from libfuse, redirect of stderr is ugly
 * use ei_* instead of erl_*? less dyn memory
 * how to remove dp_fd from cb? mark as deleted? not needed
 * errors, error on write? sigpipe?
 * eintr in callbacks.. retry or just be called again?
 * on error, wait to exit until data has been written to stdout? really needed?
 * */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <assert.h>

#include <fuse/fuse.h>
#include <fuse/fuse_lowlevel.h>

#include <ei.h>
#include <erl_interface.h>


#if 1
#define DPRINTF(f, a...)
#else
#define DPRINTF(f, a...) \
    fprintf(stderr, "DEBUG: %s:%d: " f "\n", __func__, __LINE__, ##a)
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef struct dp_fd dp_fd_t;

typedef int (dp_read_cb_t)(dp_fd_t *df);
typedef int (dp_write_cb_t)(dp_fd_t *df);
typedef int (dp_except_cb_t)(dp_fd_t *df);

enum {
    DP_READ = 1 << 0,
    DP_WRITE = 1 << 1,
    DP_EXCEPT = 1 << 2
};

struct dp_fd {
    int fd;
    int flags;
    void *opaque;
    dp_read_cb_t *read;
    dp_write_cb_t *write;
    dp_except_cb_t *except;

    int list_index;
};

dp_fd_t **dp_fd_list;
int dp_fd_list_len;
int dp_quit;

void dp_init() {
    dp_fd_list = NULL;
    dp_fd_list_len = 0;
    dp_quit = 0;
}

dp_fd_t *dp_fd_add(int fd, int flags, void *opaque,
                   dp_read_cb_t *read, dp_write_cb_t *write,
                   dp_except_cb_t *except) {
    dp_fd_t *df;

    assert(fd > -1);

    dp_fd_list = realloc(dp_fd_list, (dp_fd_list_len + 1) * sizeof(dp_fd_t*));

    df = malloc(sizeof(*df));
    df->fd = fd;
    df->flags = flags;
    df->opaque = opaque;
    df->read = read;
    df->write = write;
    df->except = except;

    df->list_index = dp_fd_list_len;

    dp_fd_list[dp_fd_list_len] = df;

    dp_fd_list_len++;

    return df;
}

void dp_fd_del(dp_fd_t *df) {
    assert(df != NULL);
    assert(df->list_index > -1);

    if(df->list_index == dp_fd_list_len - 1) {
        /* last, just free df */
    } else {
        dp_fd_t *ldf;

        ldf = dp_fd_list[dp_fd_list_len - 1];
        dp_fd_list[df->list_index] = ldf;
        ldf->list_index = df->list_index;
    }

    free(df);
    dp_fd_list = realloc(dp_fd_list, (dp_fd_list_len - 1) * sizeof(dp_fd_t*));

    dp_fd_list_len--;
}

void set_nonblocking(int fd) {
    assert(fd > -1);

    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
}

int dp_loop() {
    fd_set fdset_read, fdset_write, fdset_except;
    int i, n;
    dp_fd_t *df;
    
    for(i = 0; i < dp_fd_list_len; i++) {
        set_nonblocking(dp_fd_list[i]->fd);
    }
    
    for(;;) {
        int nfds;

        if(dp_quit)
            break;
        
        FD_ZERO(&fdset_read);
        FD_ZERO(&fdset_write);
        FD_ZERO(&fdset_except);
        
        nfds = 0;
        for(i = 0; i < dp_fd_list_len; i++) {
            df = dp_fd_list[i];
            
            if(df->flags & DP_READ)
                FD_SET(df->fd, &fdset_read);
            if(df->flags & DP_WRITE)
                FD_SET(df->fd, &fdset_write);
            if(df->flags & DP_EXCEPT)
                FD_SET(df->fd, &fdset_except);

            if(df->flags)
                nfds = MAX(nfds, df->fd);
            
            DPRINTF("fd=%d %c%c%c",
                    df->fd,
                    FD_ISSET(df->fd, &fdset_read) ? 'r' : ' ',
                    FD_ISSET(df->fd, &fdset_write) ? 'w' : ' ',
                    FD_ISSET(df->fd, &fdset_except) ? 'e' : ' ');
        }

        /* select wants highest fd + 1 */
        nfds += 1;
        
        n = select(nfds, &fdset_read, &fdset_write, &fdset_except, NULL);
        DPRINTF("select %d\n", n);
        if(n == -1) {
            if(errno == EINTR)
                continue;
            else
                return 0;
        }

        for(i = 0; i < dp_fd_list_len; i++) {
            df = dp_fd_list[i];

            if(FD_ISSET(df->fd, &fdset_read) &&
               df->read && df->read(df) != 0)
                return 0;
            if(FD_ISSET(df->fd, &fdset_write) &&
               df->write && df->write(df) != 0)
                return 0;
            if(FD_ISSET(df->fd, &fdset_except) &&
               df->except && df->except(df) != 0)
                return 0;
        }
    }

    return 0;
}


#define BUF_READ_BUFFER 32*1024
#define BUF_APPEND_CHUNK 128

typedef struct buf buf_t;

typedef int (buf_process_cb_t)(buf_t *df);

struct buf {
    char *data;
    int len;
    int used;
};


buf_t *buf_create() {
    buf_t *b;

    b = malloc(sizeof(*b));
    b->data = NULL;
    b->len = 0;
    b->used = 0;

    return b;
}

void buf_expand(buf_t *b, int len) {
    int d = len - (b->len - b->used);

    /* for speed, at lest BUF_APPEND_CHUNK or ceil to closest */
    if(d < BUF_APPEND_CHUNK)
        d = BUF_APPEND_CHUNK;
    else
        d += BUF_APPEND_CHUNK - (d % BUF_APPEND_CHUNK);

    b->len += d;
    b->data = realloc(b->data, b->len);
}

void buf_append(buf_t *b, void *data, int len) {
    buf_expand(b, len);
    memcpy(b->data + b->used, data, len);
    b->used += len;
}

void buf_remove(buf_t *b, int len) {
    memmove(b->data, b->data + len, b->used - len);
    b->used -= len;
}

/* return current position and move forward len bytes */
char *buf_use(buf_t *b, int len) {
    char *p = b->data + b->used;

    b->used += len;

    return p;
}


typedef struct forward {
    dp_fd_t *read;
    dp_fd_t *write;
    buf_t *buf;
    void *opaque;
} forward_t;


void from_fuse_message(forward_t *f, char *tag, ETERM *term) {
    uint32_t msglen, msglen_n;
    ETERM *msg;
    ETERM *elements[2];

    /* {tag, term} */
    elements[0] = erl_mk_atom(tag);
    elements[1] = term;
    msg = erl_mk_tuple(elements, 2);

    msglen = erl_term_len(msg);
    msglen_n = htonl(msglen); /* packet len in network order */
    
    DPRINTF("tag=%s msglen=%d msglen_n=%d", tag, msglen, msglen_n);
   
    /* append term len + encoded term */ 
    buf_append(f->buf, &msglen_n, sizeof(msglen_n));
    buf_expand(f->buf, msglen);
    erl_encode(msg, (unsigned char *)buf_use(f->buf, msglen));
    erl_free_term(msg);
    
    f->write->flags |= DP_WRITE;
}

int from_fuse_read(dp_fd_t *df) {
    forward_t *f = df->opaque;
    char buf[BUF_READ_BUFFER];
    uint32_t n;

    n = read(df->fd, buf, sizeof(buf));
    if(n == 0) {
        DPRINTF("close from fuse");
        return 1;
    }
    if(n == -1) {
        if(errno == ENOENT || errno == EINTR || errno == EAGAIN) {
            /* retry */
            return 0;
        }
        
        return 1;
    }

    DPRINTF("read %d byts from fuse", n);

    from_fuse_message(f, "read", erl_mk_binary(buf, n));

    return 0;
}

int from_fuse_write(dp_fd_t *df) {
    forward_t *f = df->opaque;
    int n;

    n = write(df->fd, f->buf->data, f->buf->used);
    if(n == -1 && errno == EPIPE) {
        DPRINTF("epipe from stdout");
        return 1;
    }
    if(n == -1) {
        if(errno == EINTR || errno == EAGAIN) {
            /* retry */
            return 0;
        }

        return 1;
    }

    DPRINTF("wrote %d bytes to stdout", n);

    buf_remove(f->buf, n);

    if(f->buf->used == 0)
        f->write->flags &= ~DP_WRITE;

    return 0;
}

int to_fuse_read(dp_fd_t *df) {
    forward_t *f = df->opaque;
    char buf[BUF_READ_BUFFER];
    int n;

    n = read(df->fd, buf, sizeof(buf));
    if(n == 0) {
        /* close */
        DPRINTF("close from stdin");
        return 1;
    }
    if(n == -1) {
        if(errno == EINTR || errno == EAGAIN) {
            /* retry */
            return 0;
        }

        return 1;
    }
    
    DPRINTF("read %d byts from stdin", n);

    buf_append(f->buf, buf, n);

    /* loop, there can be multiple messages */
    for(;;) {
        uint32_t l;
        int e = 0;
        forward_t *ff = f->opaque; /* from_fuse */

        if(f->buf->used < sizeof(l))
            break; /* no header, need more data */
	
	l = ntohl(*((uint32_t*)f->buf->data));
        
	if(f->buf->used - sizeof(l) < l)
	    break; /* not whole packet yet, need more data */
        
	DPRINTF("l=%d", l);
        
        /* TODO: error if n != l ? */
	/* + sizeof(l) to skip erlang packet header */
        n = write(f->write->fd, f->buf->data + sizeof(l), l);
        if(n == -1) {
            if(errno == EPIPE) {
                DPRINTF("epipe from fuse");
                return 1;
            }

            e = errno;
        } 
    
	DPRINTF("wrote %d byts to fuse", n);
        
        from_fuse_message(ff, "write", erl_mk_int(e));

	/* remove header + packet */	
        buf_remove(f->buf, sizeof(l) + l);
    }

    return 0;
}

int stderr_read(dp_fd_t *df) {
    forward_t *f = df->opaque;
    int n;
    char buf[BUF_READ_BUFFER];
    char *p;

    n = read(df->fd, buf, sizeof(buf));
    if(n == 0) {
        return 1;
    }
    if(n == -1) {
        if(errno == EINTR || errno == EAGAIN) {
            return 0;
        }

        return 1;
    }
    
    DPRINTF("read %d byts from stderr", n);

    buf_append(f->buf, buf, n);

    p = memchr(f->buf->data, '\n', f->buf->used);
    if(p) {
        forward_t *ff = f->opaque; /* from_fuse */
        int len = p - f->buf->data + 1;

        /* -1 to strip \n */
        from_fuse_message(ff, "error",
                          erl_mk_estring((const char *)f->buf->data, len - 1));
        /* not really needed? error should cause close later on */
        buf_remove(f->buf, len);
    }

    return 0;
}

void signal_quit(int signum) {
    dp_quit = 1;
}

void signal_pipe(int signum) {
    DPRINTF("");
}

int main(int argc, char **argv) {
    struct fuse_args fuse_args = FUSE_ARGS_INIT(argc, argv);
    int fuse_fd;
    struct fuse_chan *fuse_ch = NULL;
    char *mountpoint;
    int foreground;
    int multithreaded;
    forward_t from_fuse;
    forward_t to_fuse;
    forward_t from_stderr;
    int stderr_redir[2];

    /* ugly, libfuse outputs errors on stderr so redirect
     * stderr to a pipe which we read from */
    pipe(stderr_redir);
    close(STDERR_FILENO);
    fcntl(stderr_redir[1], F_DUPFD, STDERR_FILENO);
    
    signal(SIGTERM, signal_quit);
    signal(SIGINT, signal_quit);
    /* handle SIGPIPE so that write can return EPIPE */
    signal(SIGPIPE, signal_pipe);
    
    dp_init();
    erl_init(NULL, 0);

    if(fuse_parse_cmdline(&fuse_args, &mountpoint,
                       &multithreaded, &foreground) != -1) {

        fuse_ch = fuse_mount(mountpoint, &fuse_args);
        if(fuse_ch) {
            fuse_fd = fuse_chan_fd(fuse_ch);
        }
    }

    from_fuse.buf = buf_create();
    to_fuse.buf = buf_create();
    to_fuse.opaque = &from_fuse; /* to be able to append write errnos */

    /* DP_WRITE is set by from_fuse_read */
    from_fuse.write = dp_fd_add(STDOUT_FILENO, 0, &from_fuse,
                                NULL, from_fuse_write, NULL);

    /* read from stdin, append data to buffer.
     * if one or more whole fuse messages are found, write them to fuse
     * and append {write, Errno} for each message to buffer (to_fuse.opaque)
     * going to stdout */
    to_fuse.read = dp_fd_add(STDIN_FILENO, DP_READ, &to_fuse,
			     to_fuse_read, NULL, NULL);
    
    if(fuse_ch) {
        /* read from fuse, append {read, Data} on buffer used by write callback
         * going to stdout */
        from_fuse.read = dp_fd_add(fuse_fd, DP_READ, &from_fuse,
                                   from_fuse_read, NULL, NULL);
    
        /* dummy entry, only used to get fuse fd inside to_fuse_read callback */
        to_fuse.write = dp_fd_add(fuse_fd, 0, NULL, NULL, NULL, NULL);
    }
    
    from_stderr.buf = buf_create();
    from_stderr.opaque = &from_fuse;
    from_stderr.read = dp_fd_add(stderr_redir[0], DP_READ, &from_stderr,
                                 stderr_read, NULL, NULL);

    dp_loop();

    DPRINTF("dp_loop exit");
   
    if(fuse_ch) 
        fuse_unmount(mountpoint, fuse_ch);
    
    DPRINTF("done");

    return 0;
}

