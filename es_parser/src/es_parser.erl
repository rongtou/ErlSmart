%% @author rong
%% @doc 
-module(es_parser).

-export([run/1]).

run(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {_, Chunk}}]}} ->
            Result = parse(Chunk),
            io:format("~w ~n~p~n", [Mod, Result]),
            Result;
        _ ->
            ignore
    end.

parse(Chunk) ->
    parse(Chunk, #{export => [], func => []}).

parse([], Result) ->
    Result;
parse([{'attribute', _, 'export', Exports}|T], Result) ->
    parse(T, maps_append(export, Exports, Result));

parse([{function, Line, Func, Arity, Detail}|T], Result) ->
    Argus = parse_argus(Detail),
    parse(T, maps_append(func, {Func, Arity, Line, Argus}, Result));

parse([_|T], Result) ->
    parse(T, Result).

parse_argus(Detail) ->
    {clause, _, T, _, _} = hd(Detail),
    Default = lists:duplicate(length(T), 'Param'),
    lists:foldl(fun({clause, _, Argus, _, _}, Params) ->
        ArgusList = parse_argus_2(Argus, []),
        lists:zipwith(fun(X, Y) ->
            if
                X == atom -> Y;
                X == '_' -> Y;
                X == '' -> Y;
                true -> X
            end
        end, ArgusList, Params)
    end, Default, Detail).

parse_argus_2([], Acc) ->
    lists:reverse(Acc);
parse_argus_2([{atom,_,_AtomArgus}|T], Acc) ->
    parse_argus_2(T, [atom|Acc]);
parse_argus_2([{var,_,VarArgus}|T], Acc) ->
    VarArgus2 = case atom_to_list(VarArgus) of
        [$_] -> 'Param';
        [$_|Tail] -> list_to_atom(Tail);
        _ -> VarArgus
    end,
    parse_argus_2(T, [VarArgus2|Acc]);
parse_argus_2([{cons,_,_,_}|T], Acc) ->
    parse_argus_2(T, ['List'|Acc]);
parse_argus_2([{map,_,_}|T], Acc) ->
    parse_argus_2(T, ['Map'|Acc]);
parse_argus_2([_|T], Acc) ->
    parse_argus_2(T, ['Param'|Acc]).

maps_append(Key, List, Map) when is_list(List) ->
    maps:update_with(Key, fun(L) -> List ++ L end, List, Map);
maps_append(Key, Elem, Map) ->
    maps:update_with(Key, fun(L) -> [Elem | L] end, [Elem], Map).
