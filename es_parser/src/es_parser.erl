%% @author rong
%% @doc 
-module(es_parser).

-export([do/1]).

do(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {_, ChunkResult}}]}} ->
            Result = parse(ChunkResult, []),
            io:format("~w ~n~p~n", [Mod, Result]);
        _ ->
            ignore
    end.

parse([], Acc) ->
    Acc;
parse([{'attribute', _, 'export', Exports}|T], Acc) ->
    parse(T, [{export, Exports}|Acc]);

parse([{function, Line, Func, Arity, Detail}|T], Acc) ->
    Argus = parse_argus(Detail),
    parse(T, [{func, Func, Arity, Line, Argus}|Acc]);

parse([_|T], Acc) ->
    parse(T, Acc).

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

