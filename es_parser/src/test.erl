%% @author rong
%% @doc 
-module(test).

-export([start_1/0, start_2/2, start_3/2, start_4/1]).
-export([start_5/1, start_6/1]).

-record(a, {b}).

start_1() ->
    ok.

start_2(_Args1, Args2) when Args2 > 110 ->
    Args2.

start_3(argu1, _T) ->
    ok;
start_3(argu2, _A) ->
    ok;
start_3(Argus1, Argus2) ->
    Argus1 + Argus2.

start_4([]) -> ok;
start_4([_H|T]) -> start_4(T).

start_5(#a{b = 1}) -> ok.

start_6(#{b := A}) -> A.