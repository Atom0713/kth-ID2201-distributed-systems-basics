-module(loggy).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
   Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    loop(Clock, []).

loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            ClockInc = time:update(From, Time, Clock),
            Q1 = lists:keysort(2, lists:append([{From, Time, Msg}], Queue)),
            Q2 = holdbackqueue(ClockInc, Q1),
            loop(ClockInc, lists:keysort(2, Q2));
        stop ->
            io:format("queue leftovers: ~w~n", [length(Queue)]),
            ok

    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

holdbackqueue(_, []) ->
    [];
holdbackqueue(Clock, [{From, Time, Msg}|Rest]) ->
    case time:safe(Time, Clock) of
        [true, true, true, true] ->
            log(From, Time, Msg),
            holdbackqueue(Clock, Rest);
        _ ->
            holdbackqueue(Clock, Rest) ++ [{From, Time, Msg}]
    end.

