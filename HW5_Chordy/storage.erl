-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
    [].

add(Key, Value, Store) ->
    Store ++ [{Key, Value}].

lookup(Key, Store) ->
    case lists:keyfind(Key, 1, Store) of 
        {Key, Value} ->
            {Key, Value};
        false ->
            false
    end.

split(From, To, Store) ->
	lists:foldl(fun({Key, Value}, {Updated, Rest}) ->
						case node1:between(Key, From, To) of
							true ->
								{[{Key, Value} | Updated], Rest};
							false ->
								{Updated, [{Key, Value} | Rest]}
						end

					end, {[],[]}, Store).

merge(Entries, Store) ->
	lists:append(Entries, Store).