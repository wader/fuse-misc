/*
 * Ugly but working ccxstream fuse implementation
 * Mattias Wadman <mattias.wadman@gmail.com>
 *
 * How to install:
 * Install fuse (apt-get install fuse-utils libfuse2 libfuse-dev)
 * Unpack ccxstream source (http://www.xboxmediacenter.com/download_files.htm)
 * Copy this file and Makefile.ccxfuse to source directory
 * Run make -f Makefile.ccxfuse
 * 
 * How to use:
 * ./ccxfuse -o host=www.media.net,pass=arnold media
 * ./ccxfuse --help
 *
 * Notes:
 * Runs fuse in single thread mode and with a limited readahead buffer to make
 * code in ccxclient.c happy.
 * Functions prefixed with r* takes care of allocated memory, frees on
 * next call.
 * Absolute path handling is ugly, see cwd()
 *
 */

/*
 <DIRECTORYITEM>
    <NAME>Sad</NAME>
    <PATHNAME DEPTH="2">
	<COMPONENT LEVEL="1">bla</COMPONENT>
	<COMPONENT LEVEL="2">sad</COMPONENT>
    </PATHNAME>
    <ATTRIB>file</ATTRIB>
    <SIZE>0</SIZE>
    <TIME>
        <ACCESS>1174182971</ACCESS>
        <MODIFICATION>1174182971</MODIFICATION>
        <CHANGE>1174182971</CHANGE>
    </TIME>
    <INFO></INFO>
 </DIRECTORYITEM>
*/

#define FUSE_USE_VERSION 26

#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <libgen.h>
#include <stddef.h>
#include <time.h>
#include <assert.h>

#include "ccincludes.h"
#include "ccxclient.h"
#include "ccxmltrans.h"

#define ROOT_WALKUP_LEVELS 100


struct ccx_opts {
    int port;
    char *host;
    char *user;
    char *pass;
    char *root;
};

struct ccx_opts opts;
CcXstreamServerConnection ccxconn;
uid_t uid;
gid_t gid;
time_t now;


static char *rbasename(const char *path) {
    static char *d = NULL;
    char *s;

    if(d)
        free(d);
    d = strdup(path);
    s = basename(d);
    
    return s;
}

static char *rdirname(const char *path) {
    static char *d = NULL;
    char *s;

    if(d)
        free(d);
    d = strdup(path);
    s = dirname(d);
    
    return s;
}

static char *rconcat(char *start, char *text, char *stop) {
    static char *d = NULL;
    char *s;

    if(d)
	free(d);
    s = d = malloc(strlen(start) + strlen(text) + strlen(stop) + 1);
    s[0] = '\0';
    strcat(s, start);
    strcat(s, text);
    strcat(s, stop);

    return d;
}

/* this not the way to parse xml but hey */
static char *rxmltext(const char *xml, char *tag, char *def) {
    static char *d = NULL;
    char *start;
    char *stop;
    char *s;

    if(d) {
	cc_xfree(d);
	d = NULL;
    }
 
    s = rconcat("<", tag, ">");
    start = strstr(xml, s);
    if(!start)
	return def;
    start += strlen(s);

    stop = strstr(start, rconcat("</", tag, ">"));
    if(!stop)
	return def;

    s = strndup(start, stop - start);
    d = cc_xstream_xml_decode(s);
    free(s);

    return d;
}

/* ccx seams to have no set abosulute path thingi so emulate it by
 * walking up ROOT_WALKUP_LEVELS levels and then walk down into each
 * dir component. i dont like this code */
static int cwd(const char *path) {
    char *s, *n;
    char *d = strdup(rconcat(opts.root, "/", path + 1)); /* skip first "/" */
    s = d;

    if(s[0] == '/')
        s++;

    /* hopefully end up in root */
    if(cc_xstream_client_upcwd(ccxconn, ROOT_WALKUP_LEVELS) 
       != CC_XSTREAM_CLIENT_OK) {
        return 0;
    }

    /* while there are components left */
    while(strlen(s) > 0) {

        /* find first / and terminate it */    
        n = strchr(s, '/');
        if(n)
            *n = '\0';

        /* if not empty, only happens for path="/"? */
        if(strlen(s) > 0)
            if(cc_xstream_client_setcwd(ccxconn, s) !=
                                        CC_XSTREAM_CLIENT_OK) {
                free(d);
                return 0;
            }

        /* no / was found */
        if(!n)
            break;

        s = n + 1;
    }

    free(d);

    return 1;
}

static int ccx_access(const char *path, int mask)
{
    char *info;

    if(strcmp(path, "/") == 0)
        return 0;

    if(!cwd(rdirname(path))) {
        return -ENOENT;
    }

    if(cc_xstream_client_file_info(ccxconn, rbasename(path), &info) !=
                                CC_XSTREAM_CLIENT_OK) {
        return -ENOENT;
    }
    
    cc_xfree(info);
    
    return 0;
}
static int ccx_getattr(const char *path, struct stat *stbuf)
{
    char *info;
    
    memset(stbuf, 0, sizeof(struct stat));
    stbuf->st_uid = uid;
    stbuf->st_gid = gid;

    if(strcmp(path, "/") == 0) {
        stbuf->st_mode = S_IFDIR | 0755;
        stbuf->st_nlink = 3; /* . + .. + fake node */

        return 0;
    }

    if(!cwd(rdirname(path))) {
        return -ENOENT;
    }

    if(cc_xstream_client_file_info(ccxconn, rbasename(path), &info) !=
                                CC_XSTREAM_CLIENT_OK) {
        return -ENOENT;
    }
     
    if(strcmp(rxmltext(info, "ATTRIB", ""), "file") == 0) {
        stbuf->st_mode = S_IFREG | 0444;
        stbuf->st_nlink = 1;
	stbuf->st_size = atoll(rxmltext(info, "SIZE", "0"));
	/* seams to only be sent for files */
	stbuf->st_atime = atoll(rxmltext(info, "ACCESS", "0"));
	stbuf->st_mtime = atoll(rxmltext(info, "MODIFICATION", "0"));
	stbuf->st_ctime = atoll(rxmltext(info, "CHANGE", "0"));
    } else {
        stbuf->st_mode = S_IFDIR | 0755;
        stbuf->st_nlink = 3; /* . + .. + fake node */
	stbuf->st_size = 1;
	stbuf->st_atime = 
	    stbuf->st_mtime = 
	    stbuf->st_ctime = now;
    }
	
    
    cc_xfree(info);

    return 0;
}

static int ccx_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                       off_t offset, struct fuse_file_info *fi)
{
    unsigned long handle;
    char *name, *info;
    
    if(!cwd(path)) {
        return -ENOENT;
    }
    
    if(cc_xstream_client_dir_open(ccxconn, &handle) !=
       CC_XSTREAM_CLIENT_OK) {
        return -ENOTDIR;
    }
    
    while(1) {
        struct stat st;
    
        if(cc_xstream_client_dir_read(ccxconn, handle, &name, &info) !=
            CC_XSTREAM_CLIENT_OK) {
            return -ENOENT;
        }
	cc_xfree(info);

        if(strlen(name) == 0)
            return 0;
    
        memset(&st, 0, sizeof(st));

        if (filler(buf, name, &st, 0))
            break;
    }
    
    cc_xstream_client_close(ccxconn, handle);
    
    return 0;
}

typedef struct ccxfile_s
{
    unsigned long handle;
    off_t offset;
} ccxfile_t;

static int ccx_open(const char *path, struct fuse_file_info *fi)
{
    ccxfile_t *cf;
    
    cf = malloc(sizeof(ccxfile_t));
    cf->offset = 0;
    
    if(!cwd(rdirname(path))) {
        return -ENOENT;
    }

    if(cc_xstream_client_file_open(ccxconn, rbasename(path), &cf->handle) !=
       CC_XSTREAM_CLIENT_OK) {
        return -ENOENT;
    }
    
    fi->fh = (uint64_t)(uint32_t)cf;

    return 0;
}

static int ccx_read(const char *path, char *buf, size_t size, off_t offset,
                    struct fuse_file_info *fi)
{
    ccxfile_t *cf = (ccxfile_t *)(uint32_t)fi->fh;
    unsigned char *data;
    size_t data_len = 0;

    if(offset > cf->offset) {
        if(cc_xstream_client_file_forward(ccxconn, cf->handle,
                                          offset - cf->offset, 0) !=
           CC_XSTREAM_CLIENT_OK) {
            return -EINVAL;
        }
    } else if(offset < cf->offset) {
        if(cc_xstream_client_file_backwards(ccxconn, cf->handle,
                                            cf->offset - offset, 0) !=
           CC_XSTREAM_CLIENT_OK) {
            return -EINVAL;
        }
    } else {
        /* equal, no seek needed */
    }

    if(cc_xstream_client_file_read(ccxconn, cf->handle, size, &data, &data_len) !=
        CC_XSTREAM_CLIENT_OK) {
        return -EINVAL;
    }

    assert(size >= data_len);

    memcpy(buf, data, data_len);
    cc_xfree(data);

    cf->offset = offset + data_len;

    return data_len;
}

static int ccx_release(const char *path, struct fuse_file_info *fi)
{
    ccxfile_t *cf = (ccxfile_t *)(uint32_t)fi->fh;

    cc_xstream_client_close(ccxconn, cf->handle);

    free(cf);

    return 0;
}

static int ccx_statfs(const char *path, struct statvfs *stbuf)
{
    return 0;
}

static struct fuse_operations ccx_oper = {
    .getattr	= ccx_getattr,
    .access	= ccx_access,
    .readdir	= ccx_readdir,
    .open	= ccx_open,
    .read	= ccx_read,
    .statfs	= ccx_statfs,
    .release	= ccx_release
};

enum {
    KEY_HELP
};

static struct fuse_opt ccx_opts[] = {
       {"host=%s", offsetof(struct ccx_opts, host), 0},
       {"port=%u", offsetof(struct ccx_opts, port), 0},
       {"pass=%s", offsetof(struct ccx_opts, pass), 0},
       {"user=%s", offsetof(struct ccx_opts, user), 0},
       {"root=%s", offsetof(struct ccx_opts, root), 0},
       FUSE_OPT_KEY("-h", KEY_HELP),
       FUSE_OPT_KEY("--help", KEY_HELP),
       FUSE_OPT_END
};

static int ccx_opt_proc(void *data, const char *arg, int key,
                        struct fuse_args *outargs)
{
    struct ccx_opts *opts = (struct ccx_opts *)data;


    switch (key) {
    case FUSE_OPT_KEY_OPT:
        /* pass on, keep */
        return 1;
        break;
    case FUSE_OPT_KEY_NONOPT:
        /* mountpoint, keep */
        return 1;
        break;
    case KEY_HELP:
        fprintf(stderr,
                "Usage: %s mountpoint [options]\n"
                "\n"
                "CCX options:\n"
                "    -o host=HOST           server hostname (%s)\n"
                "    -o port=PORT           server port (%d)\n"
                "    -o user=USER           username (%s)\n"
                "    -o pass=PASS           password (%s)\n"
                "    -o root=PATH           path to use as root (%s)\n"
                "\n"
                "",
                outargs->argv[0],
                opts->host,
                opts->port,
                opts->user,
                opts->pass,
                opts->root
                );
        /* help with no header */
        fuse_opt_add_arg(outargs, "-ho");
        /* run main just to make it show help */
        fuse_main(outargs->argc, outargs->argv, &ccx_oper, NULL);
        exit(1);
        break;
    default:
        printf("invalid arg?\n");
        exit(1);
        break;
    }
}

int main(int argc, char *argv[])
{
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);

    uid = getuid();
    gid = getgid();
    now = time(NULL);

    opts.host = "localhost";
    opts.port = CC_XSTREAM_DEFAULT_PORT;
    opts.user = "user";
    opts.pass = "";
    opts.root = "";

    if(fuse_opt_parse(&args, &opts, ccx_opts, ccx_opt_proc) == -1) {
        exit(1);
    }

    /* normalize to non-abs */
    if(opts.root[0] == '/') {
        opts.root++;
    }

    /* not multithreaded */
    fuse_opt_add_arg(&args, "-s");
   
    /* 0x20000 (131072) hardcoded max ccx packet size (ccxclient.c)
     * set max read to max ccx packet - 100 (headers) */
    fuse_opt_add_arg(&args, "-omax_readahead=130972");

    if(cc_xstream_client_connect(opts.host, opts.port, &ccxconn) 
       != CC_XSTREAM_CLIENT_OK) {
        fprintf(stderr, "Can't connect to host %s:%d.\n", opts.host, opts.port);
        exit(1);
    }

    if (cc_xstream_client_version_handshake(ccxconn)
        != CC_XSTREAM_CLIENT_OK) {
        fprintf(stderr, "Version handshake failed.\n");
        exit(1);
    }
    
    if(cc_xstream_client_password_authenticate(ccxconn, opts.user, opts.pass)
       != CC_XSTREAM_CLIENT_OK) {
        fprintf(stderr, "Auth failed.\n");
        exit(1);
    }

    return fuse_main(args.argc, args.argv, &ccx_oper, NULL);
}
