% TODO:
% speedup: add limited reuse list
% speedup: keep track of ranges?

-module(idpool).

% api
-export([new/2, get/1, put/2]).

% test
-export([test/0]).

-record(idpool, {
        max, % id range is min to max
        min,
        next, % next id
        taken % gb_sets
        }).

new(Min, Max) ->
    #idpool{max=Max, min=Min, next=Min, taken=gb_sets:empty()}.
    
get(#idpool{max=Max, min=Min} = IdPool) ->
    get_aux(IdPool, Max - Min).
get_aux(_IdPool, Acc) when Acc < 0 ->
    false;
get_aux(#idpool{max=Max, min=Min, next=Next, taken=Taken} = IdPool,
        Acc) ->
    NewNext =
        if Next == Max -> Min;
        true -> Next + 1
        end,
    case gb_sets:is_member(Next, Taken) of
    true ->
        get_aux(IdPool#idpool{next=NewNext}, Acc - 1);
    false ->
        NewIdPool = IdPool#idpool{
            next=NewNext,
            taken=gb_sets:insert(Next, Taken)
            },
        {NewIdPool, Next}
    end.

put(#idpool{taken=Taken} = IdPool, Id) ->
    NewIdPool = IdPool#idpool{
        taken=gb_sets:delete(Id, Taken)
        },
    NewIdPool.




test1() ->
    P1 = idpool:new(1, 1000),

    % drain it
    io:fwrite("1~n"),
    P2 = lists:foldl(
            fun(_, P) ->
                {NP, _} = idpool:get(P),
                NP
                end,
            P1,
            lists:seq(1, 1000)),

    % no ids left
    io:fwrite("2~n"),
    false = idpool:get(P2),

    % put them back
    io:fwrite("3~n"),
    P3 = lists:foldl(
            fun(N, P) ->
                idpool:put(P, N)
                end,
            P2,
            lists:seq(1, 1000)),

    % drain again
    io:fwrite("4~n"),
    P4 = lists:foldl(
            fun(_, P) ->
                {NP, _} = idpool:get(P),
                NP
                end,
            P3,
            lists:seq(1, 1000)),

    % no ids left
    io:fwrite("5~n"),
    false = idpool:get(P4),

    ok.

test() ->
    test1().
 
