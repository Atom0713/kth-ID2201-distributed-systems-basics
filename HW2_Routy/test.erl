-module(test).
-export([run/0]).

run() ->
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, uppsala),
    routy:start(r4, gothenburg),
    routy:start(r5, malmo),
    r1 ! {add, lund, {r2, 'sweden@10.20.139.39'}},
    r2 ! {add, uppsala, {r3, 'sweden@10.20.139.39'}},
    r3 ! {add, gothenburg, {r4, 'sweden@10.20.139.39'}},
    r3 ! {add, malmo, {r5, 'sweden@10.20.139.39'}},
    r4 ! {add, malmo, {r5, 'sweden@10.20.139.39'}},
    r5 ! {add, stockholm, {r1, 'sweden@10.20.139.39'}},

    timer:sleep(timer:seconds(1)),
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast,

    timer:sleep(timer:seconds(1)),
    r1 ! update,
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update,

    %io:get_line("Next?\n"),
    io:format("Ask lund for status from stockholm~n"),
    r1 ! {status, {r2, 'sweden@10.20.139.39'}},

    timer:sleep(timer:seconds(1)),
    io:format("Ask stockholm for status from uppsala~n"),
    r3 ! {status, {r1, 'sweden@10.20.139.39'}},

    timer:sleep(timer:seconds(1)),
    io:format("Send message from stockholm to lund~n"),
    r1 ! {route, lund, stockholm, "Hello"},

    timer:sleep(timer:seconds(1)),
    io:format("Send message from uppsala to stockholm~n"),
    r3 ! {route, stockholm, uppsala, "Hello"},

    timer:sleep(timer:seconds(1)),
    io:format("Send message from gothenburg to stockholm~n"),
    r4 ! {route, stockholm, gothenburg, "Hello"}.