-module(fuse_high_manager).
-behavior(gen_server).

% api
-export([start/1, request/2, write_response/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("fuse_kernel.hrl").


-record(state, {
        high_module,
        fuse_handler,
        namespace_handler,
        write_response_queue,
        handlers_fh, % running fuse_high_handler processes
        handlers_pid,
        fh_idpool % idpool for fh:s
        }).


start(HighModule, Mountpoint) ->
    gen_server:start(?MODULE, [HighModule, Mountpoint], []).

request(Pid, Req) ->
    gen_server:cast(Pid, {request, Req}).

response(Pid, Rsp) ->
    gen_server:cast(Pid, {response, Rsp}).

write_response(Pid, Req) ->
    gen_server:cast(Pid, {write_response, Req}).


% get fh from request record
get_fh(read, #fuse_read_in{fh=Fh}) -> Fh;
get_fh(write, #fuse_write_in{fh=Fh}) -> Fh;
get_fh(release, #fuse_release_in{fh=Fh}) -> Fh;
get_fh(flush, #fuse_flush_in{fh=Fh}) -> Fh;
get_fh(readdir, #fuse_read_in{fh=Fh}) -> Fh;
get_fh(_Op) -> false;


init([HighModule, Mountpoint]) ->
    process_flag(trap_exit, true),
    {ok, FuseHandler} = fuse_kernel_handler:start(self(), ?MODULE, Mountpoint),
    {ok, NamespaceHandler} = fuse_high_namespace:start_link(),
    State = #state{
        high_module=HighModule,
        fuse_handler=FuseHandler,
        namespace_handler=NamespaceHandler,
        write_response_queue=queue:new(),
        handlers_fh=gb_trees:empty(),
        handlers_pid=gb_trees:empty(),
        fh_idpool=idpool:new(0, (1 bsl 64) - 1)
        },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({request, {Header, Op, Req}},
            #state{
                high_module=HighModule,
                namespace_handler=NamespaceHandler} = State) ->
    
    io:fwrite("fuse_high_manager: request ~p(~p,~p)~n", [Op, Header, Req]),
    
    {HighHandler, NewState} = 
        case get_fh(Op) of
        false ->
            io:fwrite("fuse_high_manager: no fh~n", []),
            % TODO: not all operations needs to add fh..
            {ok, HighHandler} =
                fuse_high_handler:start_link(
                    HighModule, self(), NamespaceHandler),
            {NewFhIdPool, Fh} = idpool:get(State#state.fh_idpool),
            NewPidTree =
                gb_trees:enter(HighHandler, Fh, State#state.handlers_pid),
            NewFhTree =
                gb_trees:enter(Fh, HighHandler, State#state.handlers_fh),
            
            NewState = State#state{
                fh_idpool=NewFhIdPool,
                handlers_pid=NewPidTree,
                handlers_fh=NewFhTree
                },
                
            {HighHandler, Newstate}
        Fh ->
            io:fwrite("fuse_high_manager: ~p~n", [Fh]),
            HighHandler =
                gb_trees:get(get_fh(Op, Req), State#state.handlers_fh),
            {HighHandler, State};
        end,
    
    io:fwrite("fuse_high_manager: handler ~p~n", [HighHandler]),

    fuse_high_handler:request(HighHandler, {Header, Op, Req})

    {noreply, State};

handle_cast({response, Rsp}}, State) ->
    fuse_kernel_handler:response(State#state.fuse_handler, Rsp),
    NewState = State#state{
        write_response_queue=queue:in(
        },
    {noreply, State}

handle_cast({write_response, Errno}, State) ->
    io:fwrite("fuse_high_manager: write_response: ~p", [Errno]),
    {noreply, State};

handle_cast(Request, State) ->
    io:fwrite("handle_cast: ~p", [Request]),
    {noreply, State}.

% TODO: NamespaceHandler exit?
handle_info({'EXIT', From, Reason}, State) ->
    Fh = gb_trees:get(From, State#state.handlers_pid),
    PidTree = gb_trees:delete(From, State#state.handlers_pid),
    FhTree = gb_trees:delete(Fh, State#state.handlers_fh),
    NewState = State#state{handlers_pid=PidTree, handlers_fh=FhTree},
    {noreply, NewState};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
