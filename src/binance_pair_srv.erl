%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-06 12:29:34.828589
%%%-------------------------------------------------------------------
-module(binance_pair_srv).

-behaviour(gen_server).
-include("binance.hrl").

%% API
-export([
         start_link/1,
         update/1,
         pair_to_srv_name/1,
         asks/1, asks/2,
         bids/1, bids/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          pair :: binary(),
          last_update = 1 :: pos_integer(),
          asks :: pid(),
          bids :: pid(),
          best_ask :: float(),
          best_bid :: float()
         }).

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
start_link(Pair) ->
    Id = pair_to_srv_name(Pair),
    gen_server:start_link({local, Id}, ?MODULE, [Pair], []).

update(#{ <<"s">> := Pair } = Data) ->
    gen_server:cast(pair_to_srv_name(Pair), Data);
update(Msg) ->
    lager:warning("Received unknown message from WS ~p", [Msg]).

pair_to_srv_name(Pair) ->
    list_to_atom(
      string:lowercase(
        binary_to_list(<<"binance_", Pair/bytes>>))).

asks(Pair) ->
    asks(Pair, 5).

asks(Pair, Limit) ->
    Id = pair_to_srv_name(Pair),
    gen_server:call(Id, {asks, Limit}).

bids(Pair) ->
    bids(Pair, 5).

bids(Pair, Limit) ->
    Id = pair_to_srv_name(Pair),
    gen_server:call(Id, {bids, Limit}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Pair]) ->
    {ok,
     #state{
            pair = Pair,
            asks = ets:new(asks, [ordered_set]),
            bids = ets:new(bids, [ordered_set])
           }, 
     0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({asks, Limit}, _From, #state{asks = AsksEts} = State) ->
    Reply = case ets:select(AsksEts, [{'_', [], ['$_']}], Limit) of
                '$end_of_table' ->
                    [];
                {Asks, _} ->
                    Asks
            end,
    {reply, Reply, State};
handle_call({bids, Limit}, _From, #state{bids = BidsEts} = State) ->
    Reply = case ets:select_reverse(BidsEts, [{'_', [], ['$_']}], Limit) of
                '$end_of_table' ->
                    [];
                {Bids, _} ->
                    Bids
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Cmd, State) ->
    handle_depth_command(Cmd, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, 
            #state{
               pair = Pair,
               asks = AsksEts,
               bids = BidsEts
              } = State) ->
    lager:debug("Subscribing to stream for pair: ~p", [Pair]),
    binance_ws:subscribe(Pair),
    #{ <<"asks">> := Asks,
       <<"bids">> := Bids,
       <<"lastUpdateId">> := Last
     } = binance_http_public:order_book(Pair, 1000),
    lager:debug("Depth retrieved for ~p with lastUpdateId = ~p", [Pair, Last]),
    lists:foreach(fun([Price, Volume]) ->
                          update_depth(AsksEts, Price, Volume)
                  end,
                  Asks),
    lists:foreach(fun([Price, Volume]) ->
                          update_depth(BidsEts, Price, Volume)
                  end,
                  Bids),

    {noreply, State#state{
                last_update = Last,
                best_ask = ets:first(AsksEts),
                best_bid = ets:last(BidsEts)
               }};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_depth_command(#{<<"s">> := Pair,
                       <<"b">> := Bids,
                       <<"a">> := Asks,
                       <<"U">> := First,
                       <<"u">> := Last
                      } = Update,
                     #state{
                        last_update = LU,
                        asks = AsksEts,
                        bids = BidsEts,
                        best_bid = BestBid,
                        best_ask = BestAsk
                       } = State) when LU + 1 >= First, 
                                       LU + 1 =< Last ->
    lager:debug("Depth update for ~p: ~p", [Pair, Update]),
    lists:foreach(fun([Price, Volume]) ->
                          update_depth(AsksEts, Price, Volume)
                  end,
                  Asks),
    lists:foreach(fun([Price, Volume]) ->
                          update_depth(BidsEts, Price, Volume)
                  end,
                  Bids),

    StateUpd = State#state{last_update = Last},

    case {ets:first(AsksEts), ets:last(BidsEts)} of
        {BestAsk, BestBid} ->
            {noreply, StateUpd};
        {BestAsk, NewBest} ->
            Vol = ets:lookup_element(BidsEts, NewBest, 2),
            cryptoring_amqp_exchange:publish_order_top(bid, Pair, NewBest, Vol),
            {noreply, StateUpd#state{best_bid = NewBest}};
        {NewBest, BestBid} ->
            Vol = ets:lookup_element(AsksEts, NewBest, 2),
            cryptoring_amqp_exchange:publish_order_top(ask, Pair, NewBest, Vol),
            {noreply, StateUpd#state{best_ask = NewBest}};
        {NewAsk, NewBid} ->
            AskVol = ets:lookup_element(AsksEts, NewAsk, 2),
            cryptoring_amqp_exchange:publish_order_top(ask, Pair, NewAsk, AskVol),

            BidVol = ets:lookup_element(BidsEts, NewBid, 2),
            cryptoring_amqp_exchange:publish_order_top(ask, Pair, NewBid, BidVol),

            {noreply, StateUpd#state{
                        best_ask = NewAsk,
                        best_bid = NewBid
                       }}
    end;
handle_depth_command(Command, #state{pair= Pair} = State) ->
    lager:warning("Unknown command for pair ~p: ~p", [Pair, Command]),
    {noreply, State}.

update_depth(Table, Price, Volume) when is_binary(Price) ->
    update_depth(Table, binary_to_float(Price), Volume);
update_depth(Table, Price, Volume) when is_binary(Volume) ->
    update_depth(Table, Price, binary_to_float(Volume));
update_depth(Table, Price, 0.0) ->
    ets:delete(Table, Price);
update_depth(Table, Price, Volume) ->
    ets:insert(Table, {Price, Volume}).
