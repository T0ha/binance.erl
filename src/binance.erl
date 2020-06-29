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
-export([buy/3
        ,sell/3
        ,balances/0
        ,subscribe_pair/1
        ,open_orders/0
        ,asks/2
        ,bids/2
        ,cancel_order/2
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
    Resp = binance_http_private:buy(pair_to_binance(Pair), Price, Amount),
    cryptoring_amqp_log:log(<<"order">>, Resp).

sell(Pair, Price, Amount) ->
    Resp = binance_http_private:sell(pair_to_binance(Pair), Price, Amount),
    cryptoring_amqp_log:log(<<"order">>, Resp).

balances() ->
    case binance_http_private:balances() of
        #{<<"error">> := E} ->
            lager:warning("Error getting balancies: ~p", [E]),
            Resp = #{<<"error">> => iolist_to_binary(io_lib:format("~p", [E]))},
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp;
        Balancies when is_list(Balancies) ->
            lists:foldl(fun(#{<<"coin">> := Coin, 
                              <<"free">> := Free,
                              <<"locked">> := Locked},
                            Acc) ->
                                Available = binary_to_float(Free),
                                OnOrders = binary_to_float(Locked),

                                BTCValue = price(Coin) * (Available + OnOrders),
                                Acc#{Coin => #{<<"available">> => Available,
                                               <<"onOrders">> => OnOrders,
                                               <<"btcValue">> => BTCValue
                                              }};
                           (Any, Acc) ->
                                lager:warning("Wrong coin data: ~p", [Any]),
                                Acc
                        end,
                        #{},
                        Balancies);
        E ->
            lager:warning("Error getting balancies: ~p", [E]),
            Resp = #{<<"error">> => iolist_to_binary(io_lib:format("~p", [E]))},
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp

    end.

open_orders() ->
    case binance_http_private:open_orders() of
        #{<<"error">> := E} ->
            lager:warning("Error getting balancies: ~p", [E]),
            Resp = #{<<"error">> => iolist_to_binary(io_lib:format("~p", [E]))},
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp;
        Orders when is_list(Orders) ->
            [#{<<"pair">> => pair_from_binance(Pair)
               ,<<"direction">> => string:lowercase(Direction)
               ,<<"price">> => binary_to_float(Price)
               ,<<"amount">> => binary_to_float(Amount)
               %,<<"total">> => binary_to_float(Total)
               ,<<"id">> => Id
               ,<<"timestamp">> => TS
              } || #{<<"side">> := Direction
                     ,<<"symbol">> := Pair
                     ,<<"price">> := Price
                     ,<<"origQty">> := Amount
                     ,<<"orderId">> := Id
                     %,<<"total">> := Total
                     ,<<"time">> := TS
                    } <- Orders];
        E ->
            lager:warning("Error getting balancies: ~p", [E]),
            Resp = #{<<"error">> => iolist_to_binary(io_lib:format("~p", [E]))},
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp

    end.
subscribe_pair(Pair) ->
    PairB = pair_to_binance(Pair),
    binance_pair_sup:add_pair(PairB).

asks(Pair, Limit) ->
    try binance_pair_srv:asks(pair_to_binance(Pair), Limit) of
        Result ->
            [#{<<"price">> => Price
              ,<<"amount">> => Amount
              } || {Price, Amount} <- Result]
    catch
        _:_ -> 
            []
    end.
            
bids(Pair, Limit) ->
    try binance_pair_srv:bids(pair_to_binance(Pair), Limit) of
        Result ->
            [#{<<"price">> => Price
              ,<<"amount">> => Amount
              } || {Price, Amount} <- Result]
    catch
        _:_ -> 
            []
    end.

cancel_order(Pair, OrderId) ->
    Resp = binance_http_private:cancel_order(pair_to_binance(Pair),
                                             integer_to_binary(OrderId)),

    lager:info("cancel_order('~p') = ~p", [OrderId, Resp]),
    cryptoring_amqp_log:log(<<"cancel_order">>, Resp#{<<"origOrderId">> => OrderId}).
                        
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

price(Coin) ->
    case bids(<<"BTC_", Coin/bytes>>, 1) of
        [#{<<"price">> := Price}] -> Price;
        [] -> 0.0
    end.
