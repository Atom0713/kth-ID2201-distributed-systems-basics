-module(node2).
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
    {ok, Succ} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Pred, Succ, storage:create()).

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

node(Id, Pred, Succ, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Pred, Succ, Store);
        {notify, New} ->
            {Pred2, Store2} = notify(New, Id, Pred, Store),
            node(Id, Pred2, Succ, Store2);
        {request, Peer} ->
            request(Peer, Pred),
            node(Id, Pred, Succ, Store);
        {status, Pred2} ->
            Succ2 = stabilize(Pred2, Id, Succ),
            node(Id, Pred, Succ2, Store);
        {add, Key, Value, Qref, Client} ->
            Updated = add(Key, Value, Qref, Client, Id, Pred, Succ, Store),
            node(Id, Pred, Succ, Updated);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Pred, Succ, Store),
            node(Id, Pred, Succ, Store);
        {handover, Elements} ->
            node(Id, Pred, Succ, storage:merge(Store, Elements));
        status ->
            io:format("Id ~w, Pred ~w, Succ ~w~n", [Id, Pred, Succ]),
            node(Id, Pred, Succ, Store);
        stabilize ->
            stabilize(Succ),
            node(Id, Pred, Succ, Store);
        probe ->
            create_probe(Id, Succ),
            node(Id, Pred, Succ, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Pred, Succ, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Succ),
            node(Id, Pred, Succ, Store);
        _ ->
            io:format("strage message"),
            node(Id, Pred, Succ, Store)
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
    timer:send_interval(?Stabilize, self(), stabilize).



request(Peer, Pred) ->
    case Pred of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Pred, Store) ->
    case Pred of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
				    {{Nkey, Npid}, Keep};
                false ->
                    {Pred, Store}
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


add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id)  of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
			Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Succ, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Succ,
			Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
	{Updated, Rest} = storage:split(Id, Nkey, Store),
	Npid ! {handover, Rest},
	Updated.