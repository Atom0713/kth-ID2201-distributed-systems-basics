-module(dijkstra).
-export([table/2, route/2]).

entry(Node, Sorted) ->
    % return shortest path to the Node or 0 if not found
    case lists:keyfind(Node, 1, Sorted) of
        {_, N, _} ->
            N;
        false ->
            0
    end.

replace(Node, N, Gateway, Sorted) ->
    % replaces the entry for Node in Sorted with a new entry 
    % having a new length N and Gateway. 
    % The resulting list should, of course, be sorted.
    case lists:keyfind(Node, 1, Sorted) of
        {_, _, _} ->
            S1 = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway});
        false ->
            S1 = lists:append([{Node, N, Gateway}], Sorted)
    end,
    lists:keysort(2, S1).

update(Node, N, Gateway, Sorted) ->
    N1 = entry(Node, Sorted),
    case N < N1 of
        true ->
            replace(Node, N, Gateway, Sorted);
        false ->
            Sorted
    end.

iterate(Sorted, Map, Table) ->
    case Sorted of
        [] ->
            Table;
        [{Node, N, Gateway}|R1] ->
            case N == inf of
                true ->
                    Table;
                false ->
                    Reachable_nodes = map:reachable(Node, Map),
                    case Reachable_nodes of
                        [] ->
                            R2 = R1;
                        [_|_] ->
                            R2 = update_for_each_reachable_node(Reachable_nodes, Node, R1)
                    end,
                    iterate(R2, Map, Table ++ [{Node, Gateway}])
            end
    end.
update_for_each_reachable_node([], _, _) ->
    [];
update_for_each_reachable_node([R_node], Node, Sorted) ->
    update(R_node, 1, Node, Sorted);
update_for_each_reachable_node([R_node|Rest], Node, Sorted) ->
    S2 = update(R_node, 1, Node, Sorted),
    update_for_each_reachable_node(Rest, Node, S2).



table(Gateways, Map) ->
    All_nodes = map:all_nodes(Map),
    Fun1 = fun(Node) -> {Node, inf, unknown} end,
    S1 = lists:map(Fun1, All_nodes),
    Fun2 = fun(Node) -> {Node, 0, Node} end,
    S2 = lists:map(Fun2, Gateways),
    Sorted = S2++S1,
    iterate(Sorted, Map, []).


route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {Node, Gateway} ->
            {ok, Gateway};
        false -> 
            notfound
    end.
