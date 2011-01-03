
-module(namespace).

-export([new/0,
         next_id/1,
         lookup_id/2,
         lookup_name/3,
         insert/3,
         insert/2,
         path/2,
         test/0]).

-record(node, {
        id,
        parent,
        name,
        ref_count,
        lookup_count
        }).


-record(state, {
        tree_id,
        tree_name,
        id,
        generation
        }).



new() ->
    #state{tree_id=gb_trees:empty(),
           tree_name=gb_trees:empty(),
           id=0,
           generation=0}.

% id and generation are 64 bit values, use modulus to wrap them around
% id 0 is special so skip it... FIXME root?
next_id(#state{id=Id, generation=Gen} = State) ->
    {NewId, NewGen} = 
        case (Id + 1) rem (1 bsl 64) of
        0 -> {1, (Gen + 1) rem (1 bsl 64)}; % gen overflow really needed? hmm
        N -> {N, Gen}
        end,
    {State#state{id=NewId, generation=NewGen}, {NewId, NewGen}}. 

lookup_id(#state{tree_id=TreeId}, Id) ->
    case gb_trees:lookup(Id, TreeId) of
    {value, Node} -> {ok, Node};
    none -> false
    end.

lookup_name(#state{tree_name=TreeName}, Parent, Name) ->
    case gb_trees:lookup({Parent, Name}, TreeName) of
    {value, Node} -> {ok, Node};
    none -> false
    end.
    
insert(State, #node{id=Id, parent=Parent, name=Name} = Node) ->
    case lookup_name(State, Parent, Name) of
    {ok, _Node} ->
        {ok, State};
    false ->
        NewTreeId = gb_trees:insert(Id, Node, State#state.tree_id),
        NewTreeName = gb_trees:insert({Parent, Name}, Node,
                                       State#state.tree_name),
        {ok, State#state{tree_id=NewTreeId, tree_name=NewTreeName}}
    end.

insert(State, Parent, Name) ->
    {NewState, Id} = next_id(State),
    insert(NewState, #node{id=Id, parent=Parent, name=Name,
                           ref_count=1, lookup_count=1}).



list_to_path([]) -> "/";
list_to_path(L) -> lists:flatten([["/", X] || X <- L]).

path(State, Id) ->
    path_aux(State, Id, []).
path_aux(State, Id, Acc) ->
    case lookup_id(State, Id) of
    {ok, #node{id={0,0}}} ->
        list_to_path(Acc);
    {ok, #node{parent=Parent, name=Name}} ->
        path_aux(State, Parent, [Name|Acc]);
    false ->
        false
    end.



test() ->
    N = namespace:new(),
    {ok, N1} = namespace:insert(N, #node{id={0,0}, ref_count=1, lookup_count=1}),
    {ok, N2} = namespace:insert(N1, {0, 0}, "bla"),
    {ok, N3} = namespace:insert(N2, {1, 0}, "bla1"),
    io:fwrite("lookup: ~p~n", [namespace:lookup_name(N3, {1, 0}, "bla1")]),
    
    io:fwrite("path: ~p~n", [namespace:path(N3, {2, 0})]),
    io:fwrite("path: ~p~n", [namespace:path(N3, {0, 0})]).



