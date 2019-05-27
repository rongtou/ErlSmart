-module(parserv_main).

-export([start/0]).

start() ->
    try
        application:ensure_all_started(ranch),
        application:ensure_all_started(parserv)
    catch
        _ ->
            init:stop()
    end.

