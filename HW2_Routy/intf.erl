-module(intf).
-export([add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    %add a new entry to the set and return the new set of interfaces.
    [{Name, Ref, Pid}] ++ Intf.

remove(Name, Intf) ->
%remove an entry given a name of an interface, return a new set of interfaces.
    lists:keydelete(Name, 1, Intf).


lookup(Name, Intf) ->
    %find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
    case lists:keyfind(Name, 1, Intf) of
        {_, _, Pid} ->
            {ok, Pid};
        false -> 
            notfound
    end.

ref(Name, Intf) ->
%find the reference given a name and return {ok, Ref} or notfound.
    case lists:keyfind(Name, 1, Intf) of
        {_, Ref, _} ->
            {ok, Ref};
        false -> 
            notfound
    end.

name(Ref, Intf) ->
%find the name of an entry given a reference and return {ok, Name} or notfound.
    case lists:keyfind(Ref, 2, Intf) of
        {Name, _, _} ->
            {ok, Name};
        false -> 
            notfound
    end.

list(Intf) ->
    Set = sets:from_list(extract_all_names(Intf)),
    sets:to_list(Set).

%return a list with all names.
extract_all_names([]) ->
    [];
extract_all_names([Item|RO]) ->
    {Name, _, _} = Item,
    [Name] ++ extract_all_names(RO).

broadcast(Message, Intf) ->
    All_processes = list(Intf),
    Fun2 = fun({_, Pid}) -> Pid ! Message end,
    lists:map(fun(X) -> Fun2(lookup(X, Intf)) end, All_processes).
