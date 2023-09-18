-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    % return an empty map
    [].

update(Node, Links, Map) ->
    % updates Map with directional links from that Node
    case lists:keyfind(Node, 1, Map) of
        {_, _} ->
            lists:keyreplace(Node, 1, Map, {Node, Links});
        false -> 
            lists:append([{Node, Links}], Map)
    end.

reachable(Node, Map) ->
    % return list of directional links (Nodes) directly reacheable from this Node or false.
    case lists:keyfind(Node, 1 , Map) of
        {_, Links} ->
            Links;
        false ->
            []
    end.

all_nodes(Map) ->
    % return list of all nodes in the Map
    Set = sets:from_list(extract_all_nodes(Map)),
    sets:to_list(Set).


extract_all_nodes([]) ->
    [];
extract_all_nodes([Item|RO]) ->
    {Node, Links} = Item,
    Rest = extract_all_nodes(RO),
    [Node] ++ Links ++ Rest.