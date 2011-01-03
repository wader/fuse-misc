
% in/out is from userland perspective, in is from kernel, out is to kernel
% i think fuse always use native endian
% TODO: signed/unsigned..
% TODO: document... outputs iolists..
% TODO: from fuse_kernel.h

% in header is implicit (includes opcode)
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
%              if ok fuse_getattr_out
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
% access
% in : fuse_access_in
% out: fuse_out_header
%
% create
% in : fuse_open_in, string+null
% out: fuse_out_header, fuse_entry_out, fuse_open_out


-module(fuse_kernel).

-export([open/2, close/1,
         request_read/2, request_write/2]).

-include("fuse_kernel.hrl").


open(Path, BufferSize) ->
    % TODO: fuse_forward path
    % TODO: escape " in path?
    Cmd = lists:flatten(io_lib:format("./fuse_forward \"~s\" ~p",
                                      [Path, BufferSize])),
    % use_stdio and stream is default for spawn
    case open_port({spawn, Cmd}, [exit_status, binary]) of
    Port when is_port(Port) ->
        {ok, {Port, <<>>}};
    Error ->
        {error, Error}
    end.

close({Port, _Buffer}) ->
    port_close(Port). 

% request_read(State, Data) -> {ok, [Request], State}
request_read({Port, Buffer}, Data) ->
    request_read_aux({Port, <<Buffer/binary, Data/binary>>}, []).

request_read_aux({Port, <<Len:32/native, _Rest/binary>> = Buffer}, ReqAcc) 
    when size(Buffer) >= Len ->
    <<ReqBin:Len/binary, NewBuffer/binary>> = Buffer,
    io:fwrite("read: req=~p newb=~p~n", [ReqBin, NewBuffer]),
    Req = decode(ReqBin),
    request_read_aux({Port, NewBuffer}, [Req|ReqAcc]);
request_read_aux(State, ReqAcc) ->
    {ok, lists:reverse(ReqAcc), State}.



opcode_to_atom(?FUSE_LOOKUP) -> lookup;
opcode_to_atom(?FUSE_FORGET) -> forget;
opcode_to_atom(?FUSE_GETATTR) -> getattr;
opcode_to_atom(?FUSE_SETATTR) -> setattr;
opcode_to_atom(?FUSE_READLINK) -> readlink;
opcode_to_atom(?FUSE_SYMLINK) -> symlink;
opcode_to_atom(?FUSE_MKNOD) -> mknod;
opcode_to_atom(?FUSE_MKDIR) -> mkdir;
opcode_to_atom(?FUSE_UNLINK) -> unlink;
opcode_to_atom(?FUSE_RMDIR) -> rmdir;
opcode_to_atom(?FUSE_RENAME) -> rename;
opcode_to_atom(?FUSE_LINK) -> link;
opcode_to_atom(?FUSE_OPEN) -> open;
opcode_to_atom(?FUSE_READ) -> read;
opcode_to_atom(?FUSE_WRITE) -> write;
opcode_to_atom(?FUSE_STATFS) -> statfs;
opcode_to_atom(?FUSE_RELEASE) -> release;
opcode_to_atom(?FUSE_FSYNC) -> fsync;
opcode_to_atom(?FUSE_SETXATTR) -> setxattr;
opcode_to_atom(?FUSE_GETXATTR) -> getxattr;
opcode_to_atom(?FUSE_LISTXATTR) -> listxattr;
opcode_to_atom(?FUSE_REMOVEXATTR) -> removexattr;
opcode_to_atom(?FUSE_FLUSH) -> flush;
opcode_to_atom(?FUSE_INIT) -> init;
opcode_to_atom(?FUSE_OPENDIR) -> opendir;
opcode_to_atom(?FUSE_READDIR) -> readdir;
opcode_to_atom(?FUSE_RELEASEDIR) -> releasedir;
opcode_to_atom(?FUSE_FSYNCDIR) -> fsyncdir;
opcode_to_atom(?FUSE_ACCESS) -> access;
opcode_to_atom(?FUSE_CREATE) -> create.

% decode null terminated string
decode_name0(Name) when is_binary(Name) ->
    decode_name0(binary_to_list(Name));
decode_name0(Name) ->
    lists:sublist(Name, length(Name) - 1).

% decode two concatinated null terminated strings
decode_names0(Names) ->
    L = binary_to_list(Names),
    case lists:split(string:chr(L, 0), L) of
    {Name1, Name2} ->
        {decode_name0(Name1), decode_name0(Name2)}
    end.

decode(<<Len:32/native,
         Opcode:32/native,
         Unique:64/native,
         Nodeid:64/native,
         Uid:32/native,
         Gid:32/native,
         Pid:32/native,
         _Padding:32,
         Rest/binary>>) ->
    H = #fuse_in_header{len = Len,
                        opcode = Opcode,
                        unique = Unique,
                        nodeid = Nodeid,
                        uid = Uid,
                        gid = Gid,
                        pid = Pid},
    Opatom = opcode_to_atom(Opcode),
    {H, Opatom, decode(Opatom, Rest)}.
decode(lookup, Name0) ->
    decode_name0(Name0);
decode(forget, <<Nlookup:64/native>>) ->
    #fuse_forget_in{nlookup=Nlookup};
decode(getattr, <<>>) ->
    false;
decode(setattr,
       <<Valid:32/native,
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
         _Unused43:32/native,
         Uid:32/native,
         Gid:32/native,
         _Unused5:32/native>>) ->
    #fuse_setattr_in{valid=Valid,
                     fh=Fh,
                     size=Size,
                     atime=Atime,
                     mtime=Mtime,
                     atimensec=Atimensec,
                     mtimensec=Mtimensec,
                     mode=Mode,
                     uid=Uid,
                     gid=Gid};
decode(readlink, <<>>) ->
    false;
decode(symlink, <<Name0Linkname0>>) ->
    decode_names0(Name0Linkname0);
decode(mknod,
       <<Mode:32/native,
         Rdev:32/native,
         Name/binary>>) ->
    {#fuse_mknod_in{mode=Mode,
                    rdev=Rdev},
     decode_name0(Name)};
decode(mkdir,
       <<Mode:32/native,
         _Padding:32/native,
         Name/binary>>) ->
    {#fuse_mkdir_in{mode=Mode},
     decode_name0(Name)};
decode(unlink, Name) ->
    decode_name0(Name);
decode(rmdir, Name) ->
    decode_name0(Name);
decode(rename,
       <<Newdir:64/native,
         Oldname0Newname0/binary>>) ->
    {#fuse_rename_in{newdir=Newdir},
     decode_names0(Oldname0Newname0)};
decode(link,
       <<Oldnodeid:64/native,
         Newname0/binary>>) ->
    {#fuse_link_in{oldnodeid=Oldnodeid},
     decode_name0(Newname0)};
decode(open,
       <<Flags:32/native,
         Mode:32/native>>) ->
    #fuse_open_in{flags=Flags,
		  mode=Mode};
decode(read,
       <<Fh:64/native,
         Offset:64/native,
	 Size:32/native,
         _Padding:32/native>>) ->
    #fuse_read_in{fh=Fh,
		  offset=Offset,
		  size=Size};
decode(write,
       <<Fh:64/native,
         Offset:64/native,
	 Size:32/native,
	 Writeflags:32/native,
	 Data/binary>>) ->
    {#fuse_write_in{fh=Fh,
		    offset=Offset,
		    size=Size,
		    write_flags=Writeflags},
     Data};
decode(statfs, <<>>) ->
    false;
decode(release,
       <<Fh:64/native,
         Flags:32/native,
	 _Padding:32>>) ->
    #fuse_release_in{fh=Fh,
		     flags=Flags};
decode(fsync,
       <<Fh:64/native,
         FsyncFlags:32/native,
	 _Padding:32>>) ->
    #fuse_fsync_in{fh=Fh,
		   fsync_flags=FsyncFlags};
decode(setxattr,
      <<Size:32/native,
	Flags:32/native,
	Names0/binary>>) ->
    {#fuse_setxattr_in{size=Size,
		       flags=Flags},
     decode_names0(Names0)};
decode(getxattr,
      <<Size:32/native,
	Name0/binary>>) ->
    {#fuse_getxattr_in{size=Size},
     decode_name0(Name0)};
decode(listxattr,
       <<Size:32/native,
         _Padding:32/native>>) ->
    #fuse_getxattr_in{size=Size};
decode(removexattr, <<Name0/binary>>) ->
    decode_name0(Name0);
% FIXME: u64, lockowner in 2.6?
decode(flush,
       <<Fh:64/native,
         Flushflags:32/native,
	 _Padding:32/native>>) ->
    #fuse_flush_in{fh=Fh,
                   flush_flags=Flushflags};
decode(init,
       <<Major:32/native,
         Minor:32/native,
         Maxreadahead:32/native,
         Flags:32/native>>) ->
    #fuse_init_in{major = Major,
                  minor = Minor,
                  max_readahead = Maxreadahead,
                  flags = Flags};
decode(opendir, Bin) -> % same as open
    decode(open, Bin);
decode(readdir, Bin) -> % same as readdir
    decode(read, Bin);
decode(releasedir, Bin) -> % same as releasedir
    decode(release, Bin);
decode(fsyncdir, Bin) -> % same as fsync
    decode(fsync, Bin);
decode(access,
       <<Mask:32/native,
         _Padding:32/native>>) ->
    #fuse_access_in{mask=Mask};
decode(create,
       <<Flags:32/native,
         Mode:32/native,
         Name0/binary>>) ->
    {#fuse_open_in{flags=Flags,
                   mode=Mode},
     decode_name0(Name0)}.


% TODO per opcode.. not record...

request_write({Port, _Buffer}, {Header, Rsp}) ->
    HeaderOut = encode(Header),
    RspOut = encode(Rsp),
    % +4, len inclusive
    Len = <<(iolist_size(HeaderOut) + iolist_size(RspOut) + 4):32/native>>,
    port_command(Port, [Len, HeaderOut, RspOut]).


encode(R) when is_record(R, fuse_out_header) ->
    <<(R#fuse_out_header.error):32/signed-native,
      (R#fuse_out_header.unique):64/native>>;
encode(B) when is_binary(B) ->
    B;
encode([H|_T] = L) when is_integer(H) -> % string
    L;
% encode list, takes care or [] and empty strings too
encode(L) when is_list(L) ->
    [encode(X) || X <- L];
encode(R) when is_record(R, fuse_entry_out) ->
    [<<(R#fuse_entry_out.nodeid):64/native,
       (R#fuse_entry_out.generation):64/native,
       (R#fuse_entry_out.entry_valid):64/native,
       (R#fuse_entry_out.attr_valid):64/native,
       (R#fuse_entry_out.entry_valid_nsec):32/native,
       (R#fuse_entry_out.attr_valid_nsec):32/native>>,
     encode(R#fuse_entry_out.attr)];
encode(R) when is_record(R, fuse_attr) ->
    <<(R#fuse_attr.ino):64/native,
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
      (R#fuse_attr.rdev):32/native>>;
encode(R) when is_record(R, fuse_kstatfs) ->
    <<(R#fuse_kstatfs.blocks):64/native,
      (R#fuse_kstatfs.bfree):64/native,
      (R#fuse_kstatfs.bavail):64/native,
      (R#fuse_kstatfs.files):64/native,
      (R#fuse_kstatfs.ffree):64/native,
      (R#fuse_kstatfs.bsize):32/native,
      (R#fuse_kstatfs.namelen):32/native,
      (R#fuse_kstatfs.frsize):32/native,
      0:32/native, % padding
      0:(6*8)/native>>; % spare
encode(R) when is_record(R, fuse_write_out) ->
    <<(R#fuse_write_out.size):32/native,
      0:32/native>>; % padding
encode(R) when is_record(R, fuse_open_out) ->
    <<(R#fuse_open_out.fh):64/native,
      (R#fuse_open_out.open_flags):32/native,
      0:32/native>>; % padding
encode(R) when is_record(R, fuse_init_out) ->
    <<(R#fuse_init_out.major):32/native,
      (R#fuse_init_out.minor):32/native,
      (R#fuse_init_out.max_readahead):32/native,
      (R#fuse_init_out.flags):32/native,
      0:32/native, % unused
      (R#fuse_init_out.max_write):32/native>>;
encode(R) when is_record(R, fuse_statfs_out) ->
    encode(R#fuse_statfs_out.st);
encode(R) when is_record(R, fuse_attr_out) ->
    [<<(R#fuse_attr_out.attr_valid):64/native,
       (R#fuse_attr_out.attr_valid_nsec):32/native,
       0:32/native>>, % dummy?
     encode(R#fuse_attr_out.attr)];
encode(R) when is_record(R, fuse_dirent) ->
    L = [<<(R#fuse_dirent.ino):64/native,
          (R#fuse_dirent.off):64/native,
          (R#fuse_dirent.namelen):32/native,
          (R#fuse_dirent.type):32/native>>,
         R#fuse_dirent.name],
    % 64 bit aligned
    Align = 64 - ((iolist_size(L) * 8) rem 64),
    [L, <<0:Align/native>>];
encode({R1, R2}) ->
    [encode(R1), encode(R2)].

