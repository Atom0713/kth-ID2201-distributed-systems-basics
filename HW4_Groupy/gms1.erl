-module(gms1).
-export([start/1, start/2]).

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    io:format("start node ~n"),
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) -> 
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} ->
            io:format("Invited ~n"),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.


leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            % multicast to all peers
            % send to the application layer
            io:format("ID: ~w mcast leader received Msg: ~w ~n", [Id, Msg]),
            bcast(Id, {msg, Msg}, Slaves),
            io:format("ID: ~w leader ask Master ~n", [Id]),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok

    end.

bcast(Id, Msg, Slaves) ->
    io:format("ID: ~w bcast Msg: ~wSlaves: ~w ~n", [Id, Msg, Slaves]),
    lists:map(fun(Slave) -> Slave ! Msg end, Slaves).

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            io:format("ID: ~w mcast slave received Msg: ~w ~n", [Id, Msg]),
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            io:format("ID: ~w slave received Msg: ~w ~n", [Id, Msg]),
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop ->
            ok
    end.