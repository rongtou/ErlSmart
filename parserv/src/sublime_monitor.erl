-module(sublime_monitor).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-define(SERVER, ?MODULE).

%%%-----------------------------------------------------------------------------
%%% API Functions
%%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%%%-----------------------------------------------------------------------------
%%% Callback Functions
%%%-----------------------------------------------------------------------------
init({}) ->
    erlang:send_after(1000, self(), check),
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check, State) ->
    case os:type() of
        {win32, _} ->
            Ret = os:cmd("wmic process where caption=\"sublime_text.exe\" get Handle"),
            case hd(Ret) == $H of
                true ->
                    erlang:send_after(1000, self(), check),
                    {noreply, State};
                false ->
                    init:stop(),
                    {noreply, State}
            end;
        _ ->
            Ret = os:cmd("ps -A | grep Sublime"),
            case length(string:tokens(Ret, "\n")) > 1 of
                true ->
                    erlang:send_after(1000, self(), check),
                    {noreply, State};
                false ->
                    init:stop(),
                    {noreply, State}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------------------
%%% Internal Functions
%%%-----------------------------------------------------------------------------
