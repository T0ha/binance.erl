%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-06 13:00:29.308723
%%%-------------------------------------------------------------------
-module(poloniex_pair_sup).

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

    Pairs = application:get_env(poloniex, pairs, []),

    Children = lists:map(fun(Pair) ->
                                 Id = poloniex_pair_srv:pair_to_srv_name(Pair),
                                 {Id, {'poloniex_pair_srv', start_link, [Pair]},
                                  Restart, Shutdown, Type, ['poloniex_pair_srv']}
                         end,
                         Pairs),

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
