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
         pair_to_srv_name/1
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
          bids :: pid()
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
    [ send_or_broadcast(PCode, {PCode, Command}) || Command <- Commands].

pair_to_srv_name(Pair) ->
    list_to_atom(
      binary_to_list(<<"poloniex_", Pair/bytes>>)).


                     


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
    lager:info("Max bid: ~p Min ask: ~p", [hd(Bids), hd(Asks)]),
    ets:insert(AsksEts, Asks),
    ets:insert(BidsEts, Bids),
    {noreply, State};
handle_depth_command({_PCode, [<<"o">>, 0, PriceB, VolumeB]},
                     #state{pair = Pair, asks = AsksEts} = State) ->
    lager:info("Asks update for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]),
    Price = binary_to_float(PriceB),
    case binary_to_float(VolumeB) of
        0.0 ->
            ets:delete(AsksEts, Price),
            {noreply, State};
        Volume ->
            ets:insert(AsksEts, {Price, Volume}),
            {noreply, State}
    end;
handle_depth_command({_PCode, [<<"o">>, 1, PriceB, VolumeB]},
                     #state{pair = Pair, bids = BidsEts} = State) ->
    lager:info("Bids update for ~p  price ~p volume ~p", [Pair, PriceB, VolumeB]),
    Price = binary_to_float(PriceB),
    case binary_to_float(VolumeB) of
        0.0 ->
            ets:delete(BidsEts, Price),
            {noreply, State};
        Volume ->
            ets:insert(BidsEts, {Price, Volume}),
            {noreply, State}
    end;
handle_depth_command({_PCode, [<<"t">>, _Id, 0, PriceB, VolumeB, _Timestamp]},
                     #state{pair= Pair} = State) ->
    lager:debug("Sell transaction for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]),
    {noreply, State};
handle_depth_command({_PCode, [<<"t">>, _Id, 1, PriceB, VolumeB, _Timestamp]},
                     #state{pair= Pair} = State) ->
    lager:debug("Buy transaction for ~p price ~p volume ~p", [Pair, PriceB, VolumeB]),
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
