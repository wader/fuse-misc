-module(fuse_lowlevel_test).
-behavior(gen_server).

% api
-export([start/1, request/2, write_response/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-export([init/2,
         getattr/2,
         opendir/2,
         readdir/2,
         releasedir/2,
         open/2,
         read/2,
         flush/2,
         release/2,
         lookup/2]).

-include("fuse_kernel.hrl").

-include("posix_misc.hrl").


-record(state, {
        fuse_handler
        }).


start(Mountpoint) ->
    gen_server:start(?MODULE, [Mountpoint], []).

request(Pid, Req) ->
    gen_server:cast(Pid, {request, Req}).

write_response(Pid, Req) ->
    gen_server:cast(Pid, {write_response, Req}).


init([Mountpoint]) ->
    {ok, FuseHandler} = fuse_kernel_handler:start(self(), ?MODULE, Mountpoint),
    {ok, #state{fuse_handler=FuseHandler}}.






init(_Header, Req) ->
    {ok, #fuse_init_out{
            major=Req#fuse_init_in.major,
            minor=Req#fuse_init_in.minor,
            max_readahead=Req#fuse_init_in.max_readahead,
            flags=Req#fuse_init_in.flags,
            max_write=32000}}. % TODO:

getattr(Header, _Req) ->
    case 
        case Header#fuse_in_header.nodeid of
        1 ->
            {1, 1, posix_misc:mode([dir, {all, rx}, {user, w}])}; % root
        2 ->
            {2, 123, posix_misc:mode([reg, {all, r}, {user, w}])}; % test1 file
        3 ->
            {3, 123, posix_misc:mode([reg, {user, rw}])}; % test2 file
        _ ->
            error
        end
    of
    error ->
        {error, enoent};
    {Ino, Size, Mode} -> 
        Attr = #fuse_attr{ino=Ino,
                          size=Size,
                          blocks=0,
                          atime=0,
                          mtime=0,
                          ctime=0,
                          atimensec=0,
                          mtimensec=0,
                          ctimensec=0,
                          mode=Mode,
                          nlink=2,
                          uid=1000,
                          gid=1000,
                          rdev=0},
        {ok, #fuse_attr_out{attr_valid=60,
                               attr_valid_nsec=0,
                               attr=Attr}}
    end.

opendir(Header, _Req) ->
    case Header#fuse_in_header.nodeid of
    1 ->
        R = #fuse_open_out{fh=88888,
                           open_flags=0},
        {ok, R};
    _ ->
        {error, enoent}
    end.

readdir(Header, Req) ->
    case Header#fuse_in_header.nodeid of
    1 ->
        if Req#fuse_read_in.offset == 0 ->
            {ok,
                [#fuse_dirent{ino=2, off=0, namelen=5, type=?DT_REG, name="test1"},
                 #fuse_dirent{ino=3, off=1, namelen=5, type=?DT_REG, name="test2"}]
            };
        true ->
            {ok, []}
        end;
    _ ->
        {error, enoent}
    end.

releasedir(_Header, _Req) ->
    {ok, []}.


open(Header, _Req) ->
    case Header#fuse_in_header.nodeid of
    3 ->
        {ok, #fuse_open_out{fh=0, open_flags=0}};
    _ ->
        {error, enoent}
    end.

read(Header, Req) -> 
    case Header#fuse_in_header.nodeid of
    3 ->
        Data = <<"hello\n">>,
        Offset =
            if Req#fuse_read_in.offset > size(Data) ->
                size(Data);
            true ->
                Req#fuse_read_in.offset
            end,
        Size =
            if Offset + Req#fuse_read_in.size > size(Data) -> size(Data) - Offset;
            true -> Req#fuse_read_in.size
            end,
        io:fwrite("RRRR ~p ~p ~p~n", [Offset, Size, Data]),
        <<_:Offset/binary, B:Size/binary, _/binary>> = Data,
        {ok, B};
    _ ->
        {error, enoent}
    end.

flush(Header, _Req) ->
    case Header#fuse_in_header.nodeid of
    3 ->
        {ok, []};
    _ ->
        {error, noent}
    end. 

release(Header, _Req) ->
    case Header#fuse_in_header.nodeid of
    3 ->
        {ok, []};
    _ ->
        {error, noent}
    end. 


lookup(_Header, Name) ->
    case Name of
    "test1" ->
        Attr = #fuse_attr{ino=2,
                          size=123,
                          blocks=0,
                          atime=0,
                          mtime=0,
                          ctime=0,
                          atimensec=0,
                          mtimensec=0,
                          ctimensec=0,
                          mode=posix_misc:mode([reg, {all, r}, {user, w}]),
                          nlink=2,
                          uid=1000,
                          gid=1000,
                          rdev=0},
        R = #fuse_entry_out{nodeid=2,
                            generation=0,
                            entry_valid=60,
                            attr_valid=60,
                            entry_valid_nsec=0,
                            attr_valid_nsec=0,
                            attr=Attr},
        {ok, R};
    "test2" ->
        Attr = #fuse_attr{ino=3,
                          size=123,
                          blocks=0,
                          atime=1,
                          mtime=2,
                          ctime=3,
                          atimensec=0,
                          mtimensec=0,
                          ctimensec=0,
                          mode=posix_misc:mode([reg, {user, rw}]),
                          nlink=2,
                          uid=1000,
                          gid=1000,
                          rdev=0},
        R = #fuse_entry_out{nodeid=3,
                            generation=0,
                            entry_valid=60,
                            attr_valid=60,
                            entry_valid_nsec=0,
                            attr_valid_nsec=0,
                            attr=Attr},
        {ok, R};
    _ ->
        {error, enoent}
    end.
        


















handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({request, {Header, Op, Req}}, State) ->

    io:fwrite("::::IN ~p(~p,~p)~n", [Op, Header, Req]),
    RspHeader = #fuse_out_header{unique=Header#fuse_in_header.unique},
    {RspHeader2, Rsp2} =
        case catch ?MODULE:Op(Header, Req) of
        {error, Error} ->
            {RspHeader#fuse_out_header{error=Error}, []};
        {ok, Rsp} ->
            {RspHeader#fuse_out_header{error=ok}, Rsp};
        % undef, not implemeneted... enosys
        {'EXIT', {undef, [{?MODULE, Op, _}|_]}} = Exit ->
            io:fwrite(":::EXIT ~p~n", [Exit]),
            {RspHeader#fuse_out_header{error=enosys}, []}
        % TODO: how to handle errors inside fs implementation? einval?
	% then propagate error some how?
        end,

    io:fwrite(":::OUT ~p ~p~n", [RspHeader2, Rsp2]),

    fuse_kernel_handler:response(State#state.fuse_handler,
				{RspHeader2, Rsp2}),

    {noreply, State};

%handle_cast({read, {Header, getattr, ok}}, State) ->
%

 %   fuse_kernel_handler:response(




handle_cast({write_response, Errno}, State) ->
    io:fwrite("fuse_lowlevel_test write_response: ~p", [Errno]),
    {noreply, State};


handle_cast(Request, State) ->
    io:fwrite("handle_cast: ~p", [Request]),
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
