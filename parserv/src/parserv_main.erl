-module(parserv_main).

-export([start/0]).

start() ->
    application:ensure_all_started(ranch),
    application:ensure_all_started(parserv).
