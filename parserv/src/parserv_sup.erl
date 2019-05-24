%%%-------------------------------------------------------------------
%% @doc parserv top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(parserv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
        strategy  => one_for_one,
        intensity => 10,
        period    => 5
    },
    ChildSpecs = [
        #{
            id       => sublime_monitor,
            start    => {sublime_monitor, start_link, []},
            restart  => permanent,
            shutdown => 5000,
            type     => worker,
            modules  => [sublime_monitor]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
