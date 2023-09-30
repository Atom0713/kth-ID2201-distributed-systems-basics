-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
        stop -> 
            ok
    end.


peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, LTime) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, MTime, Msg} ->
            Tinc = time:inc(Name, time:merge(LTime, MTime)),
            Log ! {log, Name, Tinc, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, Tinc);
            stop ->
                ok;
            Error ->
                Log ! {log, Name, time, {error, Error}}
            after Wait ->
                Tinc = time:inc(Name, LTime),
                Selected = select(Peers),
                Message = {hello, rand:uniform(100)},
                Selected ! {msg, Tinc, Message},
                jitter(Jitter),
                Log ! {log, Name, Tinc, {sending, Message}},
                loop(Name, Log, Peers, Sleep, Jitter, Tinc)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).