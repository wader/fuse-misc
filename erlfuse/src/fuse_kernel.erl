%
% in/out is from userland perspective, in is from kernel, out is to kernel
% i think fuse always use native endian
%
% erlang <-> fuse_forward protocol is documented in fuse_forward.c
%
% NOTE: in fuse_in_header is implicit (includes opcode)
%
% lookup
% in : string+null
% out: fuse_out_header, if ok fuse_entry_out
%
% forget:
% in : fuse_forget_in
% out: none
%
% getattr
% in : none
% out: fuse_out_header, if ok fuse_attr_out (includes fuse_attr)
%
% setattr
% in : fuse_setattr_in 
% out: fuse_out_header, if ok fuse_attr_out (includes fuse_attr)
%
% readlink
% in : none
% out: fuse_out_header, if ok string (null not appended)
%
% symlink
% in : string+null (linkname), string+null (name)
% out: fuse_out_header, if ok fuse_entry_out
%
% mknod
% in : fuse_mknod_in, string+null (name)
% out: fuse_out_header, if ok fuse_entry_out
%
% mkdir
% in : fuse_mkdir_in, string+null (name)
% out: fuse_out_header, if ok fuse_entry_out
%
% unlink
% in : string+null (name)
% out: fuse_out_header
%
% rmdir
% in : string+null (name)
% out: fuse_out_header
%
% rename
% in : fuse_rename_in, string+null (oldname), string+null (newname)
% out: fuse_out_header
%
% link
% in : fuse_link_in, string+null (name)
% out: fuse_out_header, if ok fuse_entry_out
%
% open
% in : fuse_open_in
% out: fuse_out_header, if ok fuse_open_out
%
% read
% in : fuse_read_in
% out: fuse_out_header, if ok data
%
% write
% in : fuse_write_in, data
% out: fuse_out_header, fuse_write_out
%
% statfs
% in : none
% out: fuse_out_header, if ok fuse_statfs_out
%
% release
% in : fuse_release_in
% out: fuse_out_header
%
% fsync
% in : fuse_fsync_in
% out: fuse_out_header
%
% setxattr
% in : fuse_setxattr_in, string+null (name), string+null (value)
% out: fuse_out_header
%
% getxattr
% in : fuse_getxattr_in, string+null (name)
% out: fuse_out_header
%      if ok
%          if fuse_getxattr_in.size = 0 (get attribute size)
%              fuse_getattr_out
%          else
%              string
% size should include string+null
%
% listxattr
% in : fuse_getxattr_in (same as getxattr)
% out: fuse_out_header
%      if ok
%          if fuse_getxattr_in.size = 0 (get list size)
%              fuse_getattr_out
%          else
%              list of strings
% size should include each string+null
%
% removexattr
% in : string+null
% out: fuse_out_header
%
% flush
% in : fuse_flush_in
% out: fuse_out_header
%
% init (fuse init)
% in : fuse_init_in
% out: fuse_out_header, fuse_init_out 
%
% opendir
% in : fuse_open_in
% out: fuse_out_header, if ok fuse_open_out
%
% readdir
% in : fuse_read_in
% out: fuse_out_header
%      if ok
%          list of fuse_dirent?
%
% releasedir
% in : fuse_release_in
% out: fuse_out_header
%
% fsynddir
% in : fuse_fsync_in
% out: fuse_out_header
%
% getlk
% in : fuse_lk_in
% out: fuse_out_header, fuse_lk_out
%
% setlk
% in : fuse_lk_in
% out: fuse_out_header
%
% setlkw
% in : fuse_lk_in
% out: fuse_out_header
%
% access
% in : fuse_access_in
% out: fuse_out_header
%
% create
% in : fuse_open_in, string+null
% out: fuse_out_header, fuse_entry_out, fuse_open_out
%
% interrupt
% in : fuse_interrupt_in
% out: FIXME
%
% bmap
% in : fuse_bmap_in
% out: fuse_out_header, fuse_bmap_out
%
% destroy
% in : FIXME: probably none
% out: FIXME: probably none


-module(fuse_kernel).

-export([decode/1, encode/1]).

-include("fuse_kernel.hrl").


% decode null terminated string
decode_name_null(Name) when is_binary(Name) ->
    decode_name_null(binary_to_list(Name));
decode_name_null(Name) ->
    lists:sublist(Name, length(Name) - 1).

% decode two concatinated null terminated strings
decode_name_null2(Names) ->
    L = binary_to_list(Names),
    case lists:split(string:chr(L, 0), L) of
    {Name1, Name2} ->
        {decode_name_null(Name1), decode_name_null(Name2)}
    end.

% used by encode(fuse_out_header, ...)
% note that errnos are negative
encode_errno(ok) -> 0;
encode_errno(ErrnoAtom) -> -posix_errno:atom_to_errno(ErrnoAtom).

% who much to add to get alignment
pad(N, Alignment) when N rem Alignment == 0 -> 0;
pad(N, Alignment) -> Alignment - (N rem Alignment).

% align IoList to Alignment bits
iolist_align(IoList, Alignment) ->
    [IoList, <<0:(pad(iolist_size(IoList) * 8, Alignment))>>].



% code below this line was generated using fuse_kernel.py

opcode_to_atom(1) -> lookup;
opcode_to_atom(2) -> forget;
opcode_to_atom(3) -> getattr;
opcode_to_atom(4) -> setattr;
opcode_to_atom(5) -> readlink;
opcode_to_atom(6) -> symlink;
opcode_to_atom(8) -> mknod;
opcode_to_atom(9) -> mkdir;
opcode_to_atom(10) -> unlink;
opcode_to_atom(11) -> rmdir;
opcode_to_atom(12) -> rename;
opcode_to_atom(13) -> link;
opcode_to_atom(14) -> open;
opcode_to_atom(15) -> read;
opcode_to_atom(16) -> write;
opcode_to_atom(17) -> statfs;
opcode_to_atom(18) -> release;
opcode_to_atom(20) -> fsync;
opcode_to_atom(21) -> setxattr;
opcode_to_atom(22) -> getxattr;
opcode_to_atom(23) -> listxattr;
opcode_to_atom(24) -> removexattr;
opcode_to_atom(25) -> flush;
opcode_to_atom(26) -> init;
opcode_to_atom(27) -> opendir;
opcode_to_atom(28) -> readdir;
opcode_to_atom(29) -> releasedir;
opcode_to_atom(30) -> fsyncdir;
opcode_to_atom(31) -> getlk;
opcode_to_atom(32) -> setlk;
opcode_to_atom(33) -> setlkw;
opcode_to_atom(34) -> access;
opcode_to_atom(35) -> create;
opcode_to_atom(36) -> interrupt;
opcode_to_atom(37) -> bmap;
opcode_to_atom(38) -> destroy.



decode_aux(<<Header:?FUSE_IN_HEADER_SIZE/binary, Rest/binary>> = Bin, Acc) ->
	H = decode(fuse_in_header, Header),
	Len = H#fuse_in_header.len - ?FUSE_IN_HEADER_SIZE,
	case Rest of
	<<Body:Len/binary, NewBin/binary>> ->
		Opatom = opcode_to_atom(H#fuse_in_header.opcode),
		decode_aux(NewBin, [{H, Opatom, decode(Opatom, Body)}|Acc]);
	Rest ->
		{lists:reverse(Acc), Bin}
	end;
decode_aux(Bin, Acc) ->
	{Acc, Bin}.
decode(Bin) ->
	decode_aux(Bin, []).
decode(lookup,
	<<
	Name/binary
	>>) ->
	decode_name_null(Name);
decode(forget,
	<<
	Fuse_forget_in:?FUSE_FORGET_IN_SIZE/binary
	>>) ->
	decode(fuse_forget_in, Fuse_forget_in);
decode(getattr, <<>>) ->
	ok;
decode(setattr,
	<<
	Fuse_setattr_in:?FUSE_SETATTR_IN_SIZE/binary
	>>) ->
	decode(fuse_setattr_in, Fuse_setattr_in);
decode(readlink, <<>>) ->
	ok;
decode(symlink,
	<<
	Names/binary
	>>) ->
	decode_name_null2(Names);
decode(mknod,
	<<
	Fuse_mknod_in:?FUSE_MKNOD_IN_SIZE/binary,
	Name/binary
	>>) ->
	{
	decode(fuse_mknod_in, Fuse_mknod_in),
	decode_name_null(Name)
	};
decode(mkdir,
	<<
	Fuse_mkdir_in:?FUSE_MKDIR_IN_SIZE/binary,
	Name/binary
	>>) ->
	{
	decode(fuse_mkdir_in, Fuse_mkdir_in),
	decode_name_null(Name)
	};
decode(unlink,
	<<
	Name/binary
	>>) ->
	decode_name_null(Name);
decode(rmdir,
	<<
	Name/binary
	>>) ->
	decode_name_null(Name);
decode(rename,
	<<
	Fuse_rename_in:?FUSE_RENAME_IN_SIZE/binary,
	Names/binary
	>>) ->
	{
	decode(fuse_rename_in, Fuse_rename_in),
	decode_name_null2(Names)
	};
decode(link,
	<<
	Fuse_link_in:?FUSE_LINK_IN_SIZE/binary,
	Name/binary
	>>) ->
	{
	decode(fuse_link_in, Fuse_link_in),
	decode_name_null(Name)
	};
decode(open,
	<<
	Fuse_open_in:?FUSE_OPEN_IN_SIZE/binary
	>>) ->
	decode(fuse_open_in, Fuse_open_in);
decode(read,
	<<
	Fuse_read_in:?FUSE_READ_IN_SIZE/binary
	>>) ->
	decode(fuse_read_in, Fuse_read_in);
decode(write,
	<<
	Fuse_write_in:?FUSE_WRITE_IN_SIZE/binary,
	Data/binary
	>>) ->
	{
	decode(fuse_write_in, Fuse_write_in),
	Data
	};
decode(statfs, <<>>) ->
	ok;
decode(release,
	<<
	Fuse_release_in:?FUSE_RELEASE_IN_SIZE/binary
	>>) ->
	decode(fuse_release_in, Fuse_release_in);
decode(fsync,
	<<
	Fuse_fsync_in:?FUSE_FSYNC_IN_SIZE/binary
	>>) ->
	decode(fuse_fsync_in, Fuse_fsync_in);
decode(setxattr,
	<<
	Fuse_setxattr_in:?FUSE_SETXATTR_IN_SIZE/binary,
	Names/binary
	>>) ->
	{
	decode(fuse_setxattr_in, Fuse_setxattr_in),
	decode_name_null2(Names)
	};
decode(getxattr,
	<<
	Fuse_getxattr_in:?FUSE_GETXATTR_IN_SIZE/binary,
	Name/binary
	>>) ->
	{
	decode(fuse_getxattr_in, Fuse_getxattr_in),
	decode_name_null(Name)
	};
decode(listxattr,
	<<
	Fuse_getxattr_in:?FUSE_GETXATTR_IN_SIZE/binary
	>>) ->
	decode(fuse_getxattr_in, Fuse_getxattr_in);
decode(removexattr,
	<<
	Name/binary
	>>) ->
	decode_name_null(Name);
decode(flush,
	<<
	Fuse_flush_in:?FUSE_FLUSH_IN_SIZE/binary
	>>) ->
	decode(fuse_flush_in, Fuse_flush_in);
decode(init,
	<<
	Fuse_init_in:?FUSE_INIT_IN_SIZE/binary
	>>) ->
	decode(fuse_init_in, Fuse_init_in);
decode(opendir,
	<<
	Fuse_open_in:?FUSE_OPEN_IN_SIZE/binary
	>>) ->
	decode(fuse_open_in, Fuse_open_in);
decode(readdir,
	<<
	Fuse_read_in:?FUSE_READ_IN_SIZE/binary
	>>) ->
	decode(fuse_read_in, Fuse_read_in);
decode(releasedir,
	<<
	Fuse_release_in:?FUSE_RELEASE_IN_SIZE/binary
	>>) ->
	decode(fuse_release_in, Fuse_release_in);
decode(fsyncdir,
	<<
	Fuse_fsync_in:?FUSE_FSYNC_IN_SIZE/binary
	>>) ->
	decode(fuse_fsync_in, Fuse_fsync_in);
decode(getlk,
	<<
	Fuse_lk_in:?FUSE_LK_IN_SIZE/binary
	>>) ->
	decode(fuse_lk_in, Fuse_lk_in);
decode(setlk,
	<<
	Fuse_lk_in:?FUSE_LK_IN_SIZE/binary
	>>) ->
	decode(fuse_lk_in, Fuse_lk_in);
decode(setlkw,
	<<
	Fuse_lk_in:?FUSE_LK_IN_SIZE/binary
	>>) ->
	decode(fuse_lk_in, Fuse_lk_in);
decode(access,
	<<
	Fuse_access_in:?FUSE_ACCESS_IN_SIZE/binary
	>>) ->
	decode(fuse_access_in, Fuse_access_in);
decode(create,
	<<
	Fuse_open_in:?FUSE_OPEN_IN_SIZE/binary,
	Name/binary
	>>) ->
	{
	decode(fuse_open_in, Fuse_open_in),
	decode_name_null(Name)
	};
decode(interrupt,
	<<
	Fuse_interrupt_in:?FUSE_INTERRUPT_IN_SIZE/binary
	>>) ->
	decode(fuse_interrupt_in, Fuse_interrupt_in);
decode(bmap,
	<<
	Fuse_bmap_in:?FUSE_BMAP_IN_SIZE/binary
	>>) ->
	decode(fuse_bmap_in, Fuse_bmap_in);
decode(destroy, <<>>) ->
	ok;
decode(fuse_file_lock,
	<<
	Start:64/native,
	End:64/native,
	Type:32/native,
	Pid:32/native
	>>) ->
	#fuse_file_lock{
		start=Start,
		'end'=End,
		type=Type,
		pid=Pid
	};
decode(fuse_forget_in,
	<<
	Nlookup:64/native
	>>) ->
	#fuse_forget_in{
		nlookup=Nlookup
	};
decode(fuse_mknod_in,
	<<
	Mode:32/native,
	Rdev:32/native
	>>) ->
	#fuse_mknod_in{
		mode=Mode,
		rdev=Rdev
	};
decode(fuse_mkdir_in,
	<<
	Mode:32/native,
	_Padding:32/native
	>>) ->
	#fuse_mkdir_in{
		mode=Mode
	};
decode(fuse_rename_in,
	<<
	Newdir:64/native
	>>) ->
	#fuse_rename_in{
		newdir=Newdir
	};
decode(fuse_link_in,
	<<
	Oldnodeid:64/native
	>>) ->
	#fuse_link_in{
		oldnodeid=Oldnodeid
	};
decode(fuse_setattr_in,
	<<
	Valid:32/native,
	_Padding:32/native,
	Fh:64/native,
	Size:64/native,
	_Unused1:64/native,
	Atime:64/native,
	Mtime:64/native,
	_Unused2:64/native,
	Atimensec:32/native,
	Mtimensec:32/native,
	_Unused3:32/native,
	Mode:32/native,
	_Unused4:32/native,
	Uid:32/native,
	Gid:32/native,
	_Unused5:32/native
	>>) ->
	#fuse_setattr_in{
		valid=Valid,
		fh=Fh,
		size=Size,
		atime=Atime,
		mtime=Mtime,
		atimensec=Atimensec,
		mtimensec=Mtimensec,
		mode=Mode,
		uid=Uid,
		gid=Gid
	};
decode(fuse_open_in,
	<<
	Flags:32/native,
	Mode:32/native
	>>) ->
	#fuse_open_in{
		flags=Flags,
		mode=Mode
	};
decode(fuse_release_in,
	<<
	Fh:64/native,
	Flags:32/native,
	Release_flags:32/native,
	Lock_owner:64/native
	>>) ->
	#fuse_release_in{
		fh=Fh,
		flags=Flags,
		release_flags=Release_flags,
		lock_owner=Lock_owner
	};
decode(fuse_flush_in,
	<<
	Fh:64/native,
	_Unused:32/native,
	_Padding:32/native,
	Lock_owner:64/native
	>>) ->
	#fuse_flush_in{
		fh=Fh,
		lock_owner=Lock_owner
	};
decode(fuse_read_in,
	<<
	Fh:64/native,
	Offset:64/native,
	Size:32/native,
	_Padding:32/native
	>>) ->
	#fuse_read_in{
		fh=Fh,
		offset=Offset,
		size=Size
	};
decode(fuse_write_in,
	<<
	Fh:64/native,
	Offset:64/native,
	Size:32/native,
	Write_flags:32/native
	>>) ->
	#fuse_write_in{
		fh=Fh,
		offset=Offset,
		size=Size,
		write_flags=Write_flags
	};
decode(fuse_fsync_in,
	<<
	Fh:64/native,
	Fsync_flags:32/native,
	_Padding:32/native
	>>) ->
	#fuse_fsync_in{
		fh=Fh,
		fsync_flags=Fsync_flags
	};
decode(fuse_setxattr_in,
	<<
	Size:32/native,
	Flags:32/native
	>>) ->
	#fuse_setxattr_in{
		size=Size,
		flags=Flags
	};
decode(fuse_getxattr_in,
	<<
	Size:32/native,
	_Padding:32/native
	>>) ->
	#fuse_getxattr_in{
		size=Size
	};
decode(fuse_lk_in,
	<<
	Fh:64/native,
	Owner:64/native,
	Lk:?FUSE_FILE_LOCK_SIZE/binary
	>>) ->
	#fuse_lk_in{
		fh=Fh,
		owner=Owner,
		lk=decode(fuse_file_lock, Lk)
	};
decode(fuse_access_in,
	<<
	Mask:32/native,
	_Padding:32/native
	>>) ->
	#fuse_access_in{
		mask=Mask
	};
decode(fuse_init_in,
	<<
	Major:32/native,
	Minor:32/native,
	Max_readahead:32/native,
	Flags:32/native
	>>) ->
	#fuse_init_in{
		major=Major,
		minor=Minor,
		max_readahead=Max_readahead,
		flags=Flags
	};
decode(fuse_interrupt_in,
	<<
	Unique:64/native
	>>) ->
	#fuse_interrupt_in{
		unique=Unique
	};
decode(fuse_bmap_in,
	<<
	Block:64/native,
	Blocksize:32/native,
	_Padding:32/native
	>>) ->
	#fuse_bmap_in{
		block=Block,
		blocksize=Blocksize
	};
decode(fuse_in_header,
	<<
	Len:32/native,
	Opcode:32/native,
	Unique:64/native,
	Nodeid:64/native,
	Uid:32/native,
	Gid:32/native,
	Pid:32/native,
	_Padding:32/native
	>>) ->
	#fuse_in_header{
		len=Len,
		opcode=Opcode,
		unique=Unique,
		nodeid=Nodeid,
		uid=Uid,
		gid=Gid,
		pid=Pid
	}.


encode({Header, Body}) ->
	BodyOut = encode(Body),
	Len = iolist_size(BodyOut) + ?FUSE_OUT_HEADER_SIZE,
	HeaderOut = encode(Header#fuse_out_header{len=Len}),
	[HeaderOut, BodyOut];
% pass thru binaries
encode(Bin) when is_binary(Bin) -> Bin;
% pass thru strings
encode([H|_T] = L) when is_integer(H) -> L;
% encode list of records (also takes care of empty list)
encode(L) when is_list(L) -> [encode(X) || X <- L];
encode(R) when is_record(R, fuse_attr) ->
	<<
	(R#fuse_attr.ino):64/native,
	(R#fuse_attr.size):64/native,
	(R#fuse_attr.blocks):64/native,
	(R#fuse_attr.atime):64/native,
	(R#fuse_attr.mtime):64/native,
	(R#fuse_attr.ctime):64/native,
	(R#fuse_attr.atimensec):32/native,
	(R#fuse_attr.mtimensec):32/native,
	(R#fuse_attr.ctimensec):32/native,
	(R#fuse_attr.mode):32/native,
	(R#fuse_attr.nlink):32/native,
	(R#fuse_attr.uid):32/native,
	(R#fuse_attr.gid):32/native,
	(R#fuse_attr.rdev):32/native
	>>;
encode(R) when is_record(R, fuse_kstatfs) ->
	<<
	(R#fuse_kstatfs.blocks):64/native,
	(R#fuse_kstatfs.bfree):64/native,
	(R#fuse_kstatfs.bavail):64/native,
	(R#fuse_kstatfs.files):64/native,
	(R#fuse_kstatfs.ffree):64/native,
	(R#fuse_kstatfs.bsize):32/native,
	(R#fuse_kstatfs.namelen):32/native,
	(R#fuse_kstatfs.frsize):32/native,
	0:32/native, % padding
	0:192/native % spare
	>>;
encode(R) when is_record(R, fuse_entry_out) ->
	[
	<<
	(R#fuse_entry_out.nodeid):64/native,
	(R#fuse_entry_out.generation):64/native,
	(R#fuse_entry_out.entry_valid):64/native,
	(R#fuse_entry_out.attr_valid):64/native,
	(R#fuse_entry_out.entry_valid_nsec):32/native,
	(R#fuse_entry_out.attr_valid_nsec):32/native
	>>,
	encode(R#fuse_entry_out.attr)
	];
encode(R) when is_record(R, fuse_attr_out) ->
	[
	<<
	(R#fuse_attr_out.attr_valid):64/native,
	(R#fuse_attr_out.attr_valid_nsec):32/native,
	0:32/native % dummy
	>>,
	encode(R#fuse_attr_out.attr)
	];
encode(R) when is_record(R, fuse_open_out) ->
	<<
	(R#fuse_open_out.fh):64/native,
	(R#fuse_open_out.open_flags):32/native,
	0:32/native % padding
	>>;
encode(R) when is_record(R, fuse_write_out) ->
	<<
	(R#fuse_write_out.size):32/native,
	0:32/native % padding
	>>;
encode(R) when is_record(R, fuse_statfs_out) ->
	encode(R#fuse_statfs_out.st);
encode(R) when is_record(R, fuse_getxattr_out) ->
	<<
	(R#fuse_getxattr_out.size):32/native,
	0:32/native % padding
	>>;
encode(R) when is_record(R, fuse_lk_out) ->
	encode(R#fuse_lk_out.lk);
encode(R) when is_record(R, fuse_init_out) ->
	<<
	(R#fuse_init_out.major):32/native,
	(R#fuse_init_out.minor):32/native,
	(R#fuse_init_out.max_readahead):32/native,
	(R#fuse_init_out.flags):32/native,
	0:32/native, % unused
	(R#fuse_init_out.max_write):32/native
	>>;
encode(R) when is_record(R, fuse_bmap_out) ->
	<<
	(R#fuse_bmap_out.block):64/native
	>>;
encode(R) when is_record(R, fuse_out_header) ->
	<<
	(R#fuse_out_header.len):32/native,
	(encode_errno(R#fuse_out_header.error)):32/signed-native,
	(R#fuse_out_header.unique):64/native
	>>;
encode(R) when is_record(R, fuse_dirent) ->
	iolist_align(
	[
	<<
	(R#fuse_dirent.ino):64/native,
	(R#fuse_dirent.off):64/native,
	(R#fuse_dirent.namelen):32/native,
	(R#fuse_dirent.type):32/native
	>>,
	R#fuse_dirent.name
	],
	64).
