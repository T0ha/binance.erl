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

% Internal exports
-export([
         pair_to_binance/1,
         pair_from_binance/1
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
    case binance_http_private:balances() of
        #{<<"error">> := E} = Error ->
            lager:warning("Error getting balancies: ~s", [E]),
            Error;
        Balancies ->
            lists:foldl(fun(#{<<"coin">> := Coin, 
                              <<"free">> := Free,
                              <<"locked">> := Locked},
                            Acc) ->
                                Acc#{Coin => #{<<"available">> => binary_to_float(Free),
                                               <<"onOrders">> => binary_to_float(Locked)
                                              }}
                        end,
                        #{},
                        Balancies)
    end.

subscribe_pair(Pair) ->
    PairB = pair_to_binance(Pair),
    binance_pair_sup:add_pair(PairB),
    binance_ws:subscribe(PairB).

                        
%%%===================================================================
%%% Internal functions
%%%===================================================================
pair_to_binance(Pair) ->
    [From, To] = binary:split(Pair, <<"_">>),
    <<To/bytes, From/bytes>>.

pair_from_binance(<<To:3/bytes, From:3/bytes>>) ->
    <<From/bytes, "_", To/bytes>>;
pair_from_binance(<<To:4/bytes, From:3/bytes>>) ->
    <<From/bytes, "_", To/bytes>>;
pair_from_binance(Pair) ->
    lager:error("Unexpected pair: ~p", [Pair]),
    Pair.
