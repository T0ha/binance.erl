%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-12 13:42:45.336296
%%%-------------------------------------------------------------------
-module(poloniex).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         best_bid_updated/2,
         best_ask_updated/2,
         subscribe/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("exchange.hrl").

-record(state, {
          best_price_subscribers = [] :: [pid()]
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

best_bid_updated(Pair, Price) ->
    lager:info("New best bid for ~s: ~p", [Pair, Price]),
    gen_server:cast(?SERVER, {best, bid, Pair, Price}).

best_ask_updated(Pair, Price) ->
    lager:info("New best ask for ~s: ~p", [Pair, Price]),
    gen_server:cast(?SERVER, {best, ask, Pair, Price}).

subscribe(Pid) ->
    lager:info("Subscribing ~p to best prices", [Pid]),
    gen_server:cast(?SERVER, {subscribe, Pid}).

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
    {ok, #state{}}.

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
handle_cast({subscribe, Pid}, 
            #state{
               best_price_subscribers = Subcsribers
            } = State) ->
    {noreply, State#state{best_price_subscribers = [Pid | Subcsribers]}};
handle_cast({best, Direction, Pair, Price}, 
            #state{
               best_price_subscribers = Subcsribers
            } = State) ->
    lists:foreach(fun(Pid) ->
                          Pid ! #depth{
                                   direction = Direction,
                                   price = Price,
                                   pair = bin_to_pair(Pair)
                                  }
                  end, 
                  Subcsribers),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("Wrong message: ~p in module ~p state ~p", [_Msg, ?MODULE, State]),
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
bin_to_pair(<<Quote:3/bytes, "_", Base:3/bytes>>) ->
    #pair{
       base = Base,
       quote = Quote
      }.



