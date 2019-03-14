%% @author rong
%% @doc 
-module(beam_code).

-export([start/1]).

start(File) ->
    io:format("~p~n", [beam_lib:chunks(File, [abstract_code])]).