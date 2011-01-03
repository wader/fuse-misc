-module(fuse_high_handler).
-behavior(gen_fsm).

% api
-export([start_link/3, request/2, write_response/2]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("fuse_kernel.hrl").


-record(state, {
        high_module,
        manager,
        namespace_handler,
        handler_state,
        }).


start_link(HighModule, Manager, NamespaceHandler) ->
    gen_fsm:start_link(?MODULE, [HighModule, Manager, NamespaceHandler], []).

request(Pid, Req) ->
    gen_fsm:sent_event(Pid, {request, Req}).

write_response(Pid, Req) ->
    gen_fsm:send_event(Pid, {write_response, Req}).


init([HighModule, Manager, NamespaceHandler]) ->
    State = #state{
        high_module=HighModule,
        manager=Manager,
        namespace_handler=NamespaceHandler,
        handler_state=undefined
        }
    {ok, wait_for_request, State}.


% open
% in : fuse_open_in
% out: fuse_out_header, if ok fuse_open_out
wait_for_request({request, {Hdr, open, Req},
            #state{
                high_module=HighModule,
                manager=Manager,
                namespace_handler=NS,
                handler_state=HandlerState} = State) ->
    
    Errno
        case fuse_namespace_handler:lookup(NS, Hdr#fuse_in_header.ino) of
        false ->
            {State, enoent};
        Path ->
            {ok, Pid} = HighModule:start_link(Manager, HandlerState),
            HighModule:call(Pid, {open, Path})
        end,
         
    RspHeader =
        #fuse_out_header{
            unique=Header#fuse_in_header.unique,
            errno=Errno},

    Fun =
        fun() ->            
            case fuse_high_manager:response(self(), [RspHeader])
            enoent ->
            


    {ok, wait_for_request_open_response

    % gen_server:reply from manager.. write_queue...
    case fuse_high_manager:response([RspHeader]) of
    enoent ->
        % danmn... lookup enter stuff into tree...    
    end, 

    {ok, NewState}
    

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

handle_info({'EXIT', From, Reason}, StateName, StateData) ->
    Fh = gb_trees:get(From, State#state.handlers_pid),
    PidTree = gb_trees:delete(From, State#state.handlers_pid),
    FhTree = gb_trees:delete(Fh, State#state.handlers_fh),
    NewState = State#state{handlers_pid=PidTree, handlers_fh=FhTree},
    {next_state, StateName, StateData}.

handle_info(_Request, StateName, _StateData) ->
    {next_state, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

