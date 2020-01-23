%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-12 13:42:45.336296
%%%-------------------------------------------------------------------
-module(binance).

-behaviour(cryptoring_amqp_exchange).

%% API
-export([
         start_link/0
        ]).

%% cryptoring_amqp_exchange callbacks
-export([
         buy/3,
         sell/3,
         balances/0,
         subscribe_pair/1
        ]).

-define(SERVER, ?MODULE).

-include("exchange.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    cryptoring_amqp_exchange:start_link(?MODULE).

%%%===================================================================
%%% cryptoring_amqp_exchange callbacks
%%%===================================================================
buy(Pair, Price, Amount) ->
    binance_http_private:buy(pair_to_binance(Pair), Price, Amount).

sell(Pair, Price, Amount) ->
    binance_http_private:sell(pair_to_binance(Pair), Price, Amount).

balances() ->
    binance_http_private:balances().

subscribe_pair(Pair) ->
    PairB = pair_to_binance(Pair),
    binance_pair_sup:add_pair(PairB),
    binance_ws:subscribe(PairB).

                        
%%%===================================================================
%%% Internal functions
%%%===================================================================
pair_to_binance(Pair) ->
    [From, To] = binary:split(<<"_">>),
    <<To/bytes, From/bytes>>.
