-module(parserv_handler).

-export([init/2]).

-define(mod, <<"mod">>).
-define(export, <<"export">>).
-define(func, <<"func">>).
-define(param, <<"Param">>).

init(Req0, Opts) ->
    #{path := Path} = cowboy_req:match_qs([{path, [], ""}], Req0),
    Data = parse(Path),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json; charset=utf-8">>
    }, jsx:encode(Data), Req0),
    {ok, Req, Opts}.

parse(File) ->
    case epp_dodger:quick_parse_file(File, [{clever, true}]) of
        {ok, Forms} ->
            case catch erl_syntax:revert_forms(Forms) of
                Chunks when is_list(Chunks) ->
                    Result = walk_ast(Chunks),
                    #{data => Result, code => <<"ok">>};
                Err ->
                    io:format("revert_forms error ~s~n~p~n", [File, Err]),
                    #{data => <<"">>, code => <<"revert_form_error">>}
            end;
        _ ->
            #{data => <<"">>, code => <<"parse_file_error">>}
    end.

walk_ast(Chunk) ->
    walk_ast(Chunk, #{?export => [], ?func => []}).

walk_ast([], Result) ->
    Result;
walk_ast([{attribute, _, module, Mod}|T], Result) ->
    walk_ast(T, maps:put(?mod, to_json_val(Mod), Result));

walk_ast([{attribute, _, compile, [export_all]}|T], Result) ->
    erlang:put(export_all, true),
    walk_ast(T, Result);


walk_ast([{attribute, _, export, Exports0}|T], Result) ->
    Exports = lists:map(fun({Func, Arity}) ->
        #{<<"name">> => to_json_val(Func), <<"arity">> => Arity}
    end, Exports0),
    walk_ast(T, maps_append(?export, Exports, Result));

walk_ast([{function, Line, Func, Arity, Clauses}|T], Result) ->
    Args = walk_clauses(Clauses),
    Exports = maps:get(?export, Result),
    IsExported = lists:any(fun(E) ->
      to_json_val(Func) == maps:get(<<"name">>, E)
        andalso Arity == maps:get(<<"arity">>, E)
    end , Exports),
    walk_ast(T, maps_append(?func, #{
        <<"name">>     => to_json_val(Func),
        <<"arity">>    => Arity,
        <<"line">>     => Line,
        <<"args">>     => Args,
        <<"exported">> => IsExported orelse erlang:get(export_all) == true
    }, Result));

walk_ast([_|T], Result) ->
    walk_ast(T, Result).

walk_clauses(Clauses) ->
    {clause, _, T, _, _} = hd(Clauses),
    Default = lists:duplicate(length(T), <<"Param">>),
    lists:foldl(fun({clause, _, Args, _, _}, Params) ->
        ArgsList = walk_func_args(Args, []),
        lists:zipwith(fun(X, Y) ->
            Val = if
                X == atom -> Y;
                X == '_' -> Y;
                X == '' -> Y;
                true -> X
            end,
            to_json_val(Val)
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

to_json_val(Val) when is_atom(Val) ->
    erlang:list_to_binary(erlang:atom_to_list(Val));
to_json_val(Val) when is_integer(Val) ->
    Val;
to_json_val(Val) when is_list(Val) ->
    erlang:list_to_binary(Val);
to_json_val(Val) when is_binary(Val) ->
    Val.
