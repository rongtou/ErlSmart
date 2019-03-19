%% @author rong
%% @doc 
-module(es_parser).

-export([run/1]).

run(File) ->
    case file:read_file(File) of
        {ok, FileBin} ->
            Tokens = tokens(FileBin),
            F = fun(X) ->
                case erl_parse:parse_form(X) of
                    {ok, ExprList} -> 
                        ExprList;
                    {error, _ErrorInfo} -> 
                        % try to ignore invalid syntax
                        none
                end
            end,
            Chunks = [F(X) || X <- Tokens],
            Result = walk_ast(Chunks),
            io:format("~p~n", [Result]),
            Result;
        _ ->
            none
    end.

tokens(FileBin) ->
    scan(erl_scan:tokens([], binary_to_list(FileBin), 1), []).

scan({done, {ok, T, N}, S}, Res) ->
    scan(erl_scan:tokens([], S, N), [T|Res]);
scan(_, Res) ->
    lists:reverse(Res).

walk_ast(Chunk) ->
    walk_ast(Chunk, #{export => [], func => []}).

walk_ast([], Result) ->
    Result;
walk_ast([{attribute, _, export, Exports}|T], Result) ->
    walk_ast(T, maps_append(export, Exports, Result));

walk_ast([{function, Line, Func, Arity, Clauses}|T], Result) ->
    Args = walk_clauses(Clauses),
    walk_ast(T, maps_append(func, {Func, Arity, Line, Args}, Result));

walk_ast([_|T], Result) ->
    walk_ast(T, Result).

walk_clauses(Clauses) ->
    {clause, _, T, _, _} = hd(Clauses),
    Default = lists:duplicate(length(T), 'Param'),
    lists:foldl(fun({clause, _, Args, _, _}, Params) ->
        ArgsList = walk_func_args(Args, []),
        lists:zipwith(fun(X, Y) ->
            if
                X == atom -> Y;
                X == '_' -> Y;
                X == '' -> Y;
                true -> X
            end
        end, ArgsList, Params)
    end, Default, Clauses).

walk_func_args([], Acc) ->
    lists:reverse(Acc);
walk_func_args([{atom,_,_AtomArgs}|T], Acc) ->
    walk_func_args(T, [atom|Acc]);
walk_func_args([{var,_,VarArgs}|T], Acc) ->
    VarArgs2 = case atom_to_list(VarArgs) of
        [$_] -> 'Param';
        [$_|Tail] -> list_to_atom(Tail);
        _ -> VarArgs
    end,
    walk_func_args(T, [VarArgs2|Acc]);
walk_func_args([{cons,_,_,_}|T], Acc) ->
    walk_func_args(T, ['List'|Acc]);
walk_func_args([{map,_,_}|T], Acc) ->
    walk_func_args(T, ['Map'|Acc]);
walk_func_args([_|T], Acc) ->
    walk_func_args(T, ['Param'|Acc]).

maps_append(Key, List, Map) when is_list(List) ->
    maps:update_with(Key, fun(L) -> List ++ L end, List, Map);
maps_append(Key, Elem, Map) ->
    maps:update_with(Key, fun(L) -> [Elem | L] end, [Elem], Map).
