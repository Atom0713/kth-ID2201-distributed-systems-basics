-module(gms2).
-export([start/1, start/2]).

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) -> 
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.


leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            % multicast to all peers
            % send to the application layer
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok
    end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);
        stop ->
            ok
    end.

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
    end.

bcast(Id, Msg, Nodes) ->
    lists:map(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case rand:uniform(100) of
        100 ->
            io:format("leader ~w: crash ~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.