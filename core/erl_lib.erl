#!/usr/bin/env escript

-mode(compile).

% command line exposure
main(_) ->
  io:format("~s", [code:lib_dir()]).
