%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-10-09 19:22:03.652863
%%%-------------------------------------------------------------------
-module(poloniex_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [
              {'poloniex_pair_sup', {'poloniex_pair_sup', start_link, []},
              Restart, Shutdown, supervisor, ['poloniex_pair_sup']},

              {'http_public', {'poloniex_http_public', start_link, []},
              Restart, Shutdown, Type, ['poloniex_http_public']},

              {'http_private', {'poloniex_http_private', start_link, []},
              Restart, Shutdown, Type, ['poloniex_http_private']},

              {'poloniex', {'poloniex', start_link, []},
              Restart, Shutdown, Type, ['poloniex']},

              {'ws', {'poloniex_ws', start_link, []},
              Restart, Shutdown, Type, ['poloniex_ws']}
               ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



