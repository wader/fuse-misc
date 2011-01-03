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


% code below this line was generated using fuse_kernel.py


-define(FUSE_ATTR_SIZE, 80).
-define(FUSE_KSTATFS_SIZE, 80).
-define(FUSE_FILE_LOCK_SIZE, 24).
-define(FUSE_ENTRY_OUT_SIZE, 120).
-define(FUSE_FORGET_IN_SIZE, 8).
-define(FUSE_ATTR_OUT_SIZE, 96).
-define(FUSE_MKNOD_IN_SIZE, 8).
-define(FUSE_MKDIR_IN_SIZE, 8).
-define(FUSE_RENAME_IN_SIZE, 8).
-define(FUSE_LINK_IN_SIZE, 8).
-define(FUSE_SETATTR_IN_SIZE, 88).
-define(FUSE_OPEN_IN_SIZE, 8).
-define(FUSE_OPEN_OUT_SIZE, 16).
-define(FUSE_RELEASE_IN_SIZE, 24).
-define(FUSE_FLUSH_IN_SIZE, 24).
-define(FUSE_READ_IN_SIZE, 24).
-define(FUSE_WRITE_IN_SIZE, 24).
-define(FUSE_WRITE_OUT_SIZE, 8).
-define(FUSE_STATFS_OUT_SIZE, 80).
-define(FUSE_FSYNC_IN_SIZE, 16).
-define(FUSE_SETXATTR_IN_SIZE, 8).
-define(FUSE_GETXATTR_IN_SIZE, 8).
-define(FUSE_GETXATTR_OUT_SIZE, 8).
-define(FUSE_LK_IN_SIZE, 40).
-define(FUSE_LK_OUT_SIZE, 24).
-define(FUSE_ACCESS_IN_SIZE, 8).
-define(FUSE_INIT_IN_SIZE, 16).
-define(FUSE_INIT_OUT_SIZE, 24).
-define(FUSE_INTERRUPT_IN_SIZE, 8).
-define(FUSE_BMAP_IN_SIZE, 16).
-define(FUSE_BMAP_OUT_SIZE, 8).
-define(FUSE_IN_HEADER_SIZE, 40).
-define(FUSE_OUT_HEADER_SIZE, 16).
-define(FUSE_DIRENT_SIZE, 24).


-record(fuse_attr, {
	ino, % __u64
	size, % __u64
	blocks, % __u64
	atime, % __u64
	mtime, % __u64
	ctime, % __u64
	atimensec, % __u32
	mtimensec, % __u32
	ctimensec, % __u32
	mode, % __u32
	nlink, % __u32
	uid, % __u32
	gid, % __u32
	rdev % __u32
	}).

-record(fuse_kstatfs, {
	blocks, % __u64
	bfree, % __u64
	bavail, % __u64
	files, % __u64
	ffree, % __u64
	bsize, % __u32
	namelen, % __u32
	frsize % __u32
	}).

-record(fuse_file_lock, {
	start, % __u64
	'end', % __u64
	type, % __u32
	pid % __u32, tgid
	}).

-record(fuse_entry_out, {
	nodeid, % __u64, Inode ID
	generation, % __u64, Inode generation: nodeid:gen must be unique for the fs's lifetime
	entry_valid, % __u64, Cache timeout for the name
	attr_valid, % __u64, Cache timeout for the attributes
	entry_valid_nsec, % __u32
	attr_valid_nsec, % __u32
	attr % struct fuse_attr
	}).

-record(fuse_forget_in, {
	nlookup % __u64
	}).

-record(fuse_attr_out, {
	attr_valid, % __u64, Cache timeout for the attributes
	attr_valid_nsec, % __u32
	attr % struct fuse_attr
	}).

-record(fuse_mknod_in, {
	mode, % __u32
	rdev % __u32
	}).

-record(fuse_mkdir_in, {
	mode % __u32
	}).

-record(fuse_rename_in, {
	newdir % __u64
	}).

-record(fuse_link_in, {
	oldnodeid % __u64
	}).

-record(fuse_setattr_in, {
	valid, % __u32
	fh, % __u64
	size, % __u64
	atime, % __u64
	mtime, % __u64
	atimensec, % __u32
	mtimensec, % __u32
	mode, % __u32
	uid, % __u32
	gid % __u32
	}).

-record(fuse_open_in, {
	flags, % __u32
	mode % __u32
	}).

-record(fuse_open_out, {
	fh, % __u64
	open_flags % __u32
	}).

-record(fuse_release_in, {
	fh, % __u64
	flags, % __u32
	release_flags, % __u32
	lock_owner % __u64
	}).

-record(fuse_flush_in, {
	fh, % __u64
	lock_owner % __u64
	}).

-record(fuse_read_in, {
	fh, % __u64
	offset, % __u64
	size % __u32
	}).

-record(fuse_write_in, {
	fh, % __u64
	offset, % __u64
	size, % __u32
	write_flags % __u32
	}).

-record(fuse_write_out, {
	size % __u32
	}).

-record(fuse_statfs_out, {
	st % struct fuse_kstatfs
	}).

-record(fuse_fsync_in, {
	fh, % __u64
	fsync_flags % __u32
	}).

-record(fuse_setxattr_in, {
	size, % __u32
	flags % __u32
	}).

-record(fuse_getxattr_in, {
	size % __u32
	}).

-record(fuse_getxattr_out, {
	size % __u32
	}).

-record(fuse_lk_in, {
	fh, % __u64
	owner, % __u64
	lk % struct fuse_file_lock
	}).

-record(fuse_lk_out, {
	lk % struct fuse_file_lock
	}).

-record(fuse_access_in, {
	mask % __u32
	}).

-record(fuse_init_in, {
	major, % __u32
	minor, % __u32
	max_readahead, % __u32
	flags % __u32
	}).

-record(fuse_init_out, {
	major, % __u32
	minor, % __u32
	max_readahead, % __u32
	flags, % __u32
	max_write % __u32
	}).

-record(fuse_interrupt_in, {
	unique % __u64
	}).

-record(fuse_bmap_in, {
	block, % __u64
	blocksize % __u32
	}).

-record(fuse_bmap_out, {
	block % __u64
	}).

-record(fuse_in_header, {
	len, % __u32
	opcode, % __u32
	unique, % __u64
	nodeid, % __u64
	uid, % __u32
	gid, % __u32
	pid % __u32
	}).

-record(fuse_out_header, {
	len, % __u32
	error, % __s32
	unique % __u64
	}).

-record(fuse_dirent, {
	ino, % __u64
	off, % __u64
	namelen, % __u32
	type, % __u32
	name % char
	}).

