%
% NOTE: does not work yet! this is mock-up code
%
% Passthru filesystem, same as fusexmp.c
%
% i hope posix_errno:atom_to_errno can handle all posix() atoms
%
% TODO:
% unlink collides with BIF unlink, damn
% filesystem state, file state...

-module(xmp).
-behavior(gen_server).

% api
-export([start/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0,
         init/0,
         getattr/1,
         access/2,
         readlink/1,
         readdir/1,
         mknod/3,
         mkdir/2,
         unlink/1,
         rmdir/1,
         symlink/2,
         rename/2,
         link/2,
         chmod/2,
         chown/3,
         truncate/2,
         utimens/3,
         open/1,
         read/4,
         write/4,
         release/2,
         fsync/3,
         statfs/1,
         setxattr/4,
         getxattr/2,
         listxattr/1,
         removexattr/2
         ]).

-include_lib("kernel/include/file.hrl").
-include("posix_misc.hrl"). % TODO: lib?
-include("fuse_kernel.hrl"). % TODO: lib?

% open, read, write, close state
-record(file_state, {iodevice}).

start() ->
    fuse_high_manager:start(?MODULE, ok, "bla").

start_link(Manager, State) ->
    gen_server:start_link(?MODULE, [State], []).

% gregorian is counted from 0-01-01, unix time from 1970-01-01
datetime_to_unix({Date, Time}) ->
    calendar:datetime_to_gregorian_seconds({Date, Time}) -
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

init([Manager, State]) ->
    {ok, ok}.

% getattr -> {ok, #fuseattr, NewState} | {ok, Errno, NewState}
handle_call({getattr, Path}, State) ->
    % read_link_info is like read_file_info but does not follow symlinks
    case file:read_link_info(Path) of
    {ok, FI} ->
        {ok, #fuse_attr{
                ino=FI#file_info.inode,
                size=FI#file_info.size,
                blocks=0, % TODO
                atime=datetime_to_unix(FI#file_info.atime),
                mtime=datetime_to_unix(FI#file_info.mtime),
                ctime=datetime_to_unix(FI#file_info.ctime),
                atimensec=0, % TODO
                mtimensec=0, % TODO
                ctimensec=0, % TODO
                mode=FI#file_info.mode,
                nlink=FI#file_info.links,
                uid=FI#file_info.uid,
                gid=FI#file_info.gid,
                rdev=0 % TODO
                },
            State
        };
    {error, Errno} = R ->
        {ok, Errno, State}
    end.

% access -> {ok, ok} | {ok, Errno}
handle_call({access, Path, Mask}, State) ->
    case file:read_file_info(Path) of
    {ok, FI} ->
        % TODO: execute, X_OK
        AccessMask =
            case FI#file_info.access of
            read -> ?R_OK;
            write -> ?W_OK;
            read_write -> ?R_OK bor ?W_OK;
            none -> 0
            end,
        if (?F_OK bor AccessMask) band Mask == Mask ->
            {ok, ok};
        true ->
            {ok, eacces}
        end;
    {error, Errno} = R ->
        {ok, Errno}
    end.

% readlink -> {ok, Path} | {ok, Errno}
handle_call({readlink, Path}, State} ->
    file:read_link(Path).

readdir(Path) ->
    case file:list_dir(Path) of
    {ok, Filenames} ->
        % TODO: file_info, set ino and type
        % NOTE: namelen and off fields will be set by fuse_high.. 
        {ok, [#fuse_dirent{name=File, ino=0, type=0} || File <- Filenames]};
    {error, _Errno} = R ->
        R
    end.

mknod(Path, Mode, _Rdev) ->
    case posix_misc:mode_to_type(Mode) of
    regular ->
        case file:open(Path, [write]) of
        {ok, IoDevice} ->
            file:close(IoDevice),
            % TODO: ignores error, file removed race
            case file:read_file_info(Path) of
            {ok, FI} ->
                file:write_file_info(Path,
                    FI#file_info{
                        % make sure type is regular
                        mode=((bnot ?S_IFMT) band Mode) bor ?S_IFREG});
            {error, _Errno} ->
                ok
            end,
            ok;
        {error, _Errno} = R ->
            R
        end;
    _Type -> % TODO: fifo, char, block
        {error, einval}
    end.

mkdir(Path, Mode) ->
    case file:make_dir(Path) of
    ok ->
        case file:read_file_info(Path) of
        {ok, FI} ->
            % TODO: ignores error, file removed race
            file:write_file_info(Path,
                FI#file_info{
                    % make sure type is directory
                    mode=((bnot ?S_IFMT) band Mode) bor ?S_IFDIR});
        {error, _Errno} ->
            ok
        end,
        ok;
    {error, _Errno} = R ->
        R
    end.

unlink(Path) ->
    file:delete(Path). % ok or {error, Errno}

rmdir(Path) ->
    file:del_dir(Path). % ok or {error, Errno}

symlink(From, To) ->
    file:make_symlink(From, To). % ok or {error, Errno}

rename(From, To) ->
    % TODO: not sure the semantics are the same
    file:rename(From, To). % ok or {error, Errno}

link(From, To) ->
    file:make_link(From, To). % ok or {error, Errno}

chmod(Path, Mode) ->
    case file:read_file_info(Path) of
    {ok, FI} ->
        % TODO: file removed race
        % TODO: relly needed to mask Mode?
        NewFI =
            FI#file_info{
                mode=
                    (FI#file_info.mode band ?S_IFMT) bor
                    ((bnot ?S_IFMT) band Mode)
                },
        case file:write_file_info(Path, NewFI) of
        ok ->
            ok;
        {error, _Errno} = R ->
            R
        end;
    {error, _Errno} = R ->
        R
    end.

chown(Path, Uid, Gid) ->
    case file:read_file_info(Path) of
    {ok, FI} ->
        % TODO: file removed race
        NewFI =
            FI#file_info{
                uid=Uid,
                gid=Gid
                },
        case file:write_file_info(Path, NewFI) of
        ok ->
            ok;
        {error, _Errno} = R ->
            R
        end;
    {error, _Errno} = R ->
        R
    end.

truncate(Path, Size) ->
    case file:open(Path, [write]) of
    {ok, IoDevice} ->
        % TODO: error?
        file:position(IoDevice, Size),
        file:truncate(IoDevice),
        ok;
    {error, _Errno} = R ->
        R
    end.

% like utime and utimes, but with ns resolution
utimens(Path, Atime, Mtime) ->
    case file:read_file_info(Path) of
    {ok, FI} ->
        % TODO: file removed race
        NewFI =
            FI#file_info{
                atime=Atime,
                mtime=Mtime
                },
        case file:write_file_info(Path, NewFI) of
        ok ->
            ok;
        {error, _Errno} = R ->
            R
        end;
    {error, _Errno} = R ->
        R
    end.

% open -> {error, Errno}, ok or {ok, State}
open(Path) ->
    case file:open(Path, [write]) of
    {ok, IoDevice} ->
        %file:close(IoDevice);
        {ok, #file_state{iodevice=IoDevice}};
    {error, _Errno} = R ->
        R
    end.

% read -> {error, Errno}, ok or {ok, NewState}
read(State, _Path, Size, Offset) ->
    case file:pread(State#file_state.iodevice, Offset, Size) of
    % TODO: ok even if length(Data) < Size?
    {ok, Data} = R when is_list(Data) ->
	R;
    {ok, eof} ->
	{ok, []};
    {error, _Errno} = R ->
	R
    end.

% write -> {error, Errno}, ok or {ok, NewState}
write(State, _Path, Data, Offset) ->
    case file:pwrite(State#file_state.iodevice, Offset, Data) of
    {error, _Errno} = R ->
	R;
    ok ->
	ok
    end.

release(State, _Path) ->
    case file:close(State#file_state.iodevice) of
    {error, _Errno} = R ->
	R;
    ok ->
	ok
    end.

% TODO: in file context?
fsync(_State, _Path, _IsDataSync) ->
    {error, enosys}.

statfs(_Path) ->
    {ok, #fuse_kstatfs{}}.

% TODO: flags, XATTR_CREATE, XATTR_REPLACE    
% TODO: ENOATTR, attr/xattr.h
setxattr(_Path, _Name, _Value, _Flags) ->
    {error, enosys}.

getxattr(_Path, _Name) ->
    {error, enosys}.

listxattr(_Path) ->
    {error, enosys}.

removexattr(_Path, _Name) ->
    {error, enosys}.

