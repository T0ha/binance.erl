%%%-------------------------------------------------------------------
%%% @author Anton Shvein
%%% @copyright (C) 2019, 3∑
%%% @doc
%%%
%%% @end
%%% Created : 2019-10-09 18:56:41.071742
%%%-------------------------------------------------------------------
-module(poloniex_depth).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(HEATBREAT_TIMEOUT, 2000).

-record(state, {
          pair = <<"BTC_EOS">>,
          connection,
          ref
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, Connection} = gun:open("api2.poloniex.com", 443, #{protocols => [http]}),
    lager:info("Starting ~p", [?MODULE]),
    ets:new(asks, [ordered_set, named_table]),
    ets:new(bids, [ordered_set, named_table]),
    {ok, 
     #state{
            connection = Connection
           },
    ?HEATBREAT_TIMEOUT}.

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
handle_info({gun_up, Connection, http}, #state{connection = Connection} = State) ->
    lager:info("HTTP connected ~p", [Connection]),
    Ref = gun:ws_upgrade(Connection, "/"),
    {noreply, State#state{ref = Ref}, ?HEATBREAT_TIMEOUT};
handle_info({gun_upgrade, Connection, Ref, _Protocols, _Headers},
            #state{connection = Connection, pair = Pair} = State) ->
    lager:info("WS connected ~p", [Ref]),
    Subscribe = #{
      command => <<"subscribe">>,
      channel => Pair
     },
    gun:ws_send(Connection, {text, jsx:encode(Subscribe)}),
    {noreply, State#state{ref = Ref}, ?HEATBREAT_TIMEOUT};
handle_info({gun_ws, Connection, Ref, {text, <<"[1010]">>}},
            #state{connection = Connection, ref = Ref} = State) ->
    lager:debug("Heatbreat"),
    {noreply, State, ?HEATBREAT_TIMEOUT};
handle_info({gun_ws, Connection, Ref, {text, Data}},
            #state{
               connection = Connection,
               ref = Ref,
               pair = Pair
              } = State) ->
    lager:debug("Received data for ~p: ~p", [Pair, Data]),
    Json = jsx:decode(Data),
    handle_depth_data(Pair, Json),
    {noreply, State, ?HEATBREAT_TIMEOUT};
handle_info(timeout, State) ->
    lager:debug("Timeout"),
    {noreply, State, ?HEATBREAT_TIMEOUT};
handle_info(_Info, State) ->
    lager:warning("Wrong message: ~p in module ~p", [_Info, ?MODULE]),
    {noreply, State, ?HEATBREAT_TIMEOUT}.

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
handle_depth_data(Pair, [PCode, Sequence, Commands]) ->
    [ handle_depth_command(Pair, Command) || Command <- Commands].

handle_depth_command(Pair, [<<"i">>, [_, {<<"orderBook">>, [Ask, Bid]}]]) ->
    lager:debug("Bids: ~p~nAsks: ~p~n", [Bid, Ask]),
    Bids = [ {binary_to_float(P), binary_to_float(V)} || {P, V} <- Bid],
    Asks = [ {binary_to_float(P), binary_to_float(V)} || {P, V} <- Ask],
    lager:info("Max bid: ~p Min ask: ~p", [hd(Bids), hd(Asks)]),
    ets:insert(asks, Asks),
    ets:insert(bids, Bids);
handle_depth_command(Pair, [<<"o">>, 0, PriceB, VolumeB]) ->
    lager:info("Asks update for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]),
    Price = binary_to_float(PriceB),
    case binary_to_float(VolumeB) of
        0.0 ->
            ets:delete(asks, Price);
        Volume ->
            ets:insert(asks, {Price, Volume})
    end;
handle_depth_command(Pair, [<<"o">>, 1, PriceB, VolumeB]) ->
    lager:info("Bids update for ~p  price ~p volume ~p", [Pair, PriceB, VolumeB]),
    Price = binary_to_float(PriceB),
    case binary_to_float(VolumeB) of
        0.0 ->
            ets:delete(bids, Price);
        Volume ->
            ets:insert(bids, {Price, Volume})
    end;
handle_depth_command(Pair, [<<"t">>, _Id, 0, PriceB, VolumeB, _Timestamp]) ->
    lager:debug("Sell transaction for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]);
handle_depth_command(Pair, [<<"t">>, _Id, 1, PriceB, VolumeB, _Timestamp]) ->
    lager:debug("Buy transaction for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]);
handle_depth_command(Pair, Command) ->
    lager:warning("Unknown command for pair ~p: ~p", [Pair, Command]).




