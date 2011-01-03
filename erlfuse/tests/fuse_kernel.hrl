
-define(FUSE_LOOKUP, 1).
-define(FUSE_FORGET, 2).
-define(FUSE_GETATTR, 3).
-define(FUSE_SETATTR, 4).
-define(FUSE_READLINK, 5).
-define(FUSE_SYMLINK, 6).
-define(FUSE_MKNOD, 8).
-define(FUSE_MKDIR, 9).
-define(FUSE_UNLINK, 10).
-define(FUSE_RMDIR, 11).
-define(FUSE_RENAME, 12).
-define(FUSE_LINK, 13).
-define(FUSE_OPEN, 14).
-define(FUSE_READ, 15).
-define(FUSE_WRITE, 16).
-define(FUSE_STATFS, 17).
-define(FUSE_RELEASE, 18).
-define(FUSE_FSYNC, 20).
-define(FUSE_SETXATTR, 21).
-define(FUSE_GETXATTR, 22).
-define(FUSE_LISTXATTR, 23).
-define(FUSE_REMOVEXATTR, 24).
-define(FUSE_FLUSH, 25).
-define(FUSE_INIT, 26).
-define(FUSE_OPENDIR, 27).
-define(FUSE_READDIR, 28).
-define(FUSE_RELEASEDIR, 29).
-define(FUSE_FSYNCDIR, 30).
-define(FUSE_GETLK, 31).
-define(FUSE_SETLK, 32).
-define(FUSE_SETLKW, 33).
-define(FUSE_ACCESS, 34).
-define(FUSE_CREATE, 35).
-define(FUSE_INTERRUPT, 36).
-define(FUSE_BMAP, 37).
-define(FUSE_DESTROY, 38).

% setattr_in.valid
-define(FATTR_MODE,     (1 bsl 0)).
-define(FATTR_UID,      (1 bsl 1)).
-define(FATTR_GID,      (1 bsl 2)).
-define(FATTR_SIZE,     (1 bsl 3)).
-define(FATTR_ATIME,    (1 bsl 4)).
-define(FATTR_MTIME,    (1 bsl 5)).
-define(FATTR_FH,       (1 bsl 6)).

% open request flags
% TODO: in or out? both?
-define(FUSE_DIRECT_IO,     (1 bsl 0)).
-define(FUSE_KEEP_CACHE,    (1 bsl 1)).


% init_in/out flags
-define(FUSE_ASYNC_READ,    (1 bsl 0)).
-define(FUSE_POSIX_LOCKS,   (1 bsl 1)).

% release_in flags
-define(FUSE_RELEASE_FLUSH, (1 bsl 0)).

-record(fuse_attr, {
        ino,
        size,
        blocks,
        atime,
        mtime,
        ctime,
        atimensec,
        mtimensec,
        ctimensec,
        mode,
        nlink,
        uid,
        gid,
        rdev
        }).


-record(fuse_kstatfs, {
	blocks,
	bfree,
	bavail,
	files,
	ffree,
	bsize,
	namelen,
	frsize
	}).

-record(fuse_file_lock, {
        start,
        'end',
        type,
        pid
        }).

-record(fuse_dirent, {
        ino,
        off, % offset to next dirent.. how, and alignment?
        namelen,
        type,
        name
        }).


-record(fuse_entry_out, {
        nodeid,
        generation,
        entry_valid,
        attr_valid,
        entry_valid_nsec,
        attr_valid_nsec,
        attr
        }).


-record(fuse_in_header, {
        len,
        opcode,
        unique,
        nodeid,
        uid,
        gid,
        pid
        }).
-record(fuse_out_header, {
        error,
        unique
        }).


-record(fuse_init_in, {
        major,
        minor,
        max_readahead,
        flags
        }).
-record(fuse_init_out, {
        major,
        minor,
        max_readahead,
        flags,
        max_write
        }).


-record(fuse_forget_in, {
        nlookup
        }).


-record(fuse_setattr_in, {
        valid,
        fh,
        size,
        atime,
        mtime,
        atimensec,
        mtimensec,
        mode,
        uid,
        gid
        }).


-record(fuse_mknod_in, {
        mode,
        rdev
        }).


-record(fuse_mkdir_in, {
        mode
        }).


-record(fuse_rename_in, {
        newdir
        }).


-record(fuse_link_in, {
	oldnodeid
	}).


-record(fuse_open_in, {
	flags,
	mode
	}).
-record(fuse_open_out, {
	fh,
	open_flags
	}).


-record(fuse_read_in, {
	fh,
	offset,
	size
	}).


-record(fuse_write_in, {
	fh,
	offset,
	size,
	write_flags
	}).
-record(fuse_write_out, {
        size
        }).

-record(fuse_statfs_out, {
	st
	}).


-record(fuse_release_in, {
	fh,
	flags,
        release_flags,
        lock_owner
	}).


-record(fuse_fsync_in, {
	fh,
	fsync_flags
	}).


-record(fuse_setxattr_in, {
	size,
	flags
	}).


-record(fuse_getxattr_in, {
	size
	}).
-record(fuse_getxattr_out, {
        size,
        padding
        }).

-record(fuse_flush_in, {
	fh,
	flush_flags,
	lock_owner
	}).

-record(fuse_lk_in, {
        fh,
        owner,
        lk 
        }).
-record(fuse_lk_out, {
        lk 
        }).

-record(fuse_access_in, {
        mask
        }).


-record(fuse_interrupt_in, {
        unique
        }).


-record(fuse_bmap_in, {
        block,
        blocksize
        }).
-record(fuse_bmap_out, {
        block
        }).


% used by getattr and setattr
-record(fuse_attr_out, {
        attr_valid,
        attr_valid_nsec,
        % dummy?
        attr
        }).

