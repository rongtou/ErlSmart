-module(parserv_main).

-export([start/0]).

start() ->
    case gen_tcp:connect("127.0.0.1", 48437, []) of
        {ok, _} ->
            erlang:halt();
        _ ->
            application:ensure_all_started(ranch),
            application:ensure_all_started(parserv)
    end.
