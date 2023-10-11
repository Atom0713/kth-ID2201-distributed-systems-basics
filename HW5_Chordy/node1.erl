-module(node1).
-export([start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Pred = nil,
    io:format("connect to Peer: ~w ~n", [Peer]),
    {ok, Succ} = connect(Id, Peer),
    io:format("connected. Succ: ~w ~n", [Succ]),
    schedule_stabilize(),
    node(Id, Pred, Succ).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
    end.

node(Id, Pred, Succ) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Pred, Succ);
        {notify, New} ->
            Pred2 = notify(New, Id, Pred),
            node(Id, Pred2, Succ);
        {request, Peer} ->
            request(Peer, Pred),
            node(Id, Pred, Succ);
        {status, Pred2} ->
            Succ2 = stabilize(Pred2, Id, Succ),
            node(Id, Pred, Succ2);
        status ->
            io:format("Id ~w, Pred ~w, Succ ~w~n", [Id, Pred, Succ]),
            node(Id, Pred, Succ);
        stabilize ->
            stabilize(Succ),
            node(Id, Pred, Succ);
        probe ->
            create_probe(Id, Succ),
            node(Id, Pred, Succ);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Pred, Succ);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Succ),
            node(Id, Pred, Succ);
        _ ->
            io:format("strage message"),
            node(Id, Pred, Succ)
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.
stabilize(Pred, Id, Succ) ->
    {Skey, Spid} = Succ,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Succ;
        {Id, _} ->
            Succ;
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Succ;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {notify, {Id, self()}},
                    Pred;
                false ->
                    Spid ! {notify, {Id, self()}},
                    Succ
            end
    end.

schedule_stabilize() ->
    %io:format("schedule trigger stabilize~n"),
    timer:send_interval(?Stabilize, self(), stabilize).



request(Peer, Pred) ->
    case Pred of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Pred) ->
    case Pred of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    {Nkey, Npid};
                false ->
                    Pred
            end
    end.



create_probe(Id, {_, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
    Spid ! {probe, Ref, Nodes ++ [Id], T}.


remove_probe(T, Nodes) ->
    Func = fun(N) -> io:format("~w, ",[N]) end,
    lists:map(Func, Nodes),
    io:format("~nTime = ~w~n",[erlang:system_time(micro_seconds) - T]).
