-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(_, T) ->
    T+1.

merge(Ti, Tj) ->
    case leq(Ti, Tj) of
        true ->
            Tj;
        false ->
            Ti
    end.

leq(Ti, Tj) ->
    Ti =< Tj. 

clock(Nodes) ->
    lists:map(fun(Node) -> {Node, 0} end, Nodes).

update(Node, Time, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) ->
    lists:map(fun({_, LatestT}) -> leq(Time, LatestT) end, Clock).

