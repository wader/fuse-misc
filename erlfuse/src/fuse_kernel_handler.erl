-module(fuse_kernel_handler).
-behavior(gen_server).

% api
-export([start/3, stop/1, response/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("fuse_kernel.hrl").


-record(state, {
        handler,
        mod,
	fuse_port,
        read_bin
        }).


start(Handler, Mod, Mountpoint) ->
    gen_server:start(?MODULE, [Handler, Mod, Mountpoint], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

response(Pid, Rsp) ->
    gen_server:cast(Pid, {response, Rsp}).


init([Handler, Mod, Mountpoint]) ->
    Cmd = lists:flatten(io_lib:format("fuse_forward \"~s\"",
                                      [Mountpoint])),
    % use_stdio is default for spawn
    case open_port({spawn, Cmd}, [{packet, 4}, exit_status, binary]) of
    Port when is_port(Port) ->
        {ok, #state{handler=Handler, mod=Mod, read_bin= <<>>,
                    fuse_port=Port}};
    Error ->
        {stop, Error}
    end.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({response, Rsp}, #state{fuse_port=FusePort} = State) ->
    io:fwrite("respons: ~p~n", [Rsp]),
    port_command(FusePort, fuse_kernel:encode(Rsp)),
    {noreply, State};

handle_cast(stop, State) ->
    io:fwrite("stop:~n", []),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}},
            #state{fuse_port=Port, mod=Mod, handler=Handler} = State) ->
    case binary_to_term(Data) of
    {read, Bin} ->
        {Reqs, NewReadBin} =
            fuse_kernel:decode(<<(State#state.read_bin)/binary, Bin/binary>>),
        % Reqs can be [] (need more data) then decode will just append binary
        % and foreach will do nothing
        lists:foreach(
            fun(Req) -> Mod:request(Handler, Req) end,
            Reqs),
        io:fwrite("data read: Reqs=~p~n", [Reqs]),
        {noreply, State#state{read_bin=NewReadBin}};
    {write, Errno} ->
        ErrnoAtom =
            case Errno of
            0 ->
                ok;
            Errno ->
                posix_errno:errno_to_atom(Errno)
            end,
	Mod:write_response(Handler, ErrnoAtom),
        io:fwrite("data write: Errno=~p~n", [Errno]),
        {noreply, State};
    {error, Error} ->
        io:fwrite("data error: Error=~p~n", [Error]),
        {stop, {fuse_forward_error, Error}, State}
    end;

handle_info({Port, {exit_status, ExitCode}}, #state{fuse_port=Port} = State) ->
    {stop, {fuse_forward_exit, ExitCode}, State#state{fuse_port=undefined}};

% TODO: error writing to port, fatal?
handle_info({'EXIT', Port, PosixCode}, #state{fuse_port=Port} = State) ->
    {stop, {fuse_forward_write_error, PosixCode}, State};

handle_info(Request, State) ->
    io:fwrite("handle_info: ~p ~p~n", [Request, State]),
    {noreply, State}.


terminate(_Reason, #state{fuse_port=FusePort}) when is_port(FusePort) ->
    port_close(FusePort),
    ok;
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
