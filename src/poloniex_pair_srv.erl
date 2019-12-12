%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-06 12:29:34.828589
%%%-------------------------------------------------------------------
-module(poloniex_pair_srv).

-behaviour(gen_server).
-include("poloniex.hrl").

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
          code :: pos_integer(),
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

update([PCode, _Sequence, Commands]) ->
    [ send_or_broadcast(PCode, {PCode, Command}) || Command <- Commands];
update(Msg) ->
    lager:warning("Received unknown message from WS ~p", [Msg]).

pair_to_srv_name(Pair) ->
    list_to_atom(
      binary_to_list(<<"poloniex_", Pair/bytes>>)).

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
     2000}.

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
handle_cast(Cmd, #state{pair = Pair} = State) ->
    handle_depth_command(Cmd, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(timeout, #state{pair = Pair} = State) ->
    poloniex_ws:subscribe(Pair),
    {noreply, State};
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
handle_depth_command({PCode, [<<"i">>, [{<<"currencyPair">>, Pair}, {<<"orderBook">>, [Ask, Bid]}]]},
                     #state{
                        asks = AsksEts,
                        pair = Pair,
                        bids = BidsEts
                       } = State) ->
    lager:debug("Bids: ~p~nAsks: ~p~n", [Bid, Ask]),
    ets:insert(poloniex_pairs, [#pair{
                                   code = PCode,
                                   pair = Pair,
                                   pid = self()
                                  }]),
    Bids = [ {binary_to_float(P), binary_to_float(V)} || {P, V} <- Bid],
    Asks = [ {binary_to_float(P), binary_to_float(V)} || {P, V} <- Ask],
    lager:debug("Max bid: ~p Min ask: ~p", [hd(Bids), hd(Asks)]),
    ets:insert(AsksEts, Asks),
    ets:insert(BidsEts, Bids),
    {noreply, State#state{
                code = PCode,
                best_ask = element(1, hd(Asks)),
                best_bid = element(1, hd(Bids))
               }};
handle_depth_command({_PCode, [<<"i">>, [{<<"currencyPair">>, Pair}, _]]},
                     #state{
                        pair = OurPair
                       } = State) when Pair /= OurPair ->
    {noreply, State};
handle_depth_command({PCode, [<<"o">>, 0, Price, Volume]},
                     #state{
                        pair = Pair,
                        code = PCode,
                        asks = AsksEts,
                        best_ask = BestPrice
                       } = State) ->
    lager:debug("Asks update for ~p price ~p volume ~p", [Pair, Price, Volume]),
    update_depth(AsksEts, Price, Volume),
    case ets:first(AsksEts) of
        BestPrice ->
            {noreply, State};
        NewBest ->
            poloniex:best_ask_updated(Pair, NewBest),
            {noreply, State#state{best_ask = NewBest}}
    end;
handle_depth_command({PCode, [<<"o">>, 1, Price, Volume]},
                     #state{
                        pair = Pair,
                        code = PCode,
                        best_bid = BestPrice,
                        bids = BidsEts
                       } = State) ->
    lager:debug("Bids update for ~p  price ~p volume ~p", [Pair, Price, Volume]),
    update_depth(BidsEts, Price, Volume),
    case ets:last(BidsEts) of
        BestPrice ->
            {noreply, State};
        NewBest ->
            poloniex:best_bid_updated(Pair, NewBest),
            {noreply, State#state{best_bid = NewBest}}
    end;
handle_depth_command({_PCode, [<<"t">>, _Id, 0, PriceB, VolumeB, _Timestamp]},
                     #state{pair= Pair} = State) ->
    lager:info("Sell transaction for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]),
    {noreply, State};
handle_depth_command({_PCode, [<<"t">>, _Id, 1, PriceB, VolumeB, _Timestamp]},
                     #state{pair= Pair} = State) ->
    lager:info("Buy transaction for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]),
    {noreply, State};
handle_depth_command({PCode, _},
                     #state{
                        code = OurPCode
                       } = State) when PCode /= OurPCode ->
    {noreply, State};
handle_depth_command(Command, #state{pair= Pair} = State) ->
    lager:warning("Unknown command for pair ~p: ~p", [Pair, Command]),
    {noreply, State}.

send_or_broadcast(PCode, Data) ->
    case ets:lookup(poloniex_pairs, PCode) of
        [] -> 
            lists:foreach(fun({_, Pid, _, _}) ->
                                  gen_server:cast(Pid, Data)
                          end,
                          supervisor:which_children(poloniex_pair_sup));
        [#pair{pid = Pid}] ->
            gen_server:cast(Pid, Data)
    end.

update_depth(Table, Price, Volume) when is_binary(Price) ->
    update_depth(Table, binary_to_float(Price), Volume);
update_depth(Table, Price, Volume) when is_binary(Volume) ->
    update_depth(Table, Price, binary_to_float(Volume));
update_depth(Table, Price, 0.0) ->
    ets:delete(Table, Price);
update_depth(Table, Price, Volume) ->
    ets:insert(Table, {Price, Volume}).
