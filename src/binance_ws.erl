%%%-------------------------------------------------------------------
%%% @author Anton Shvein
%%% @copyright (C) 2019, 3∑
%%% @doc
%%%
%%% @end
%%% Created : 2019-10-09 18:56:41.071742
%%%-------------------------------------------------------------------
-module(binance_ws).

-behaviour(gen_server).

-include("binance.hrl").

%% API
-export([
         start_link/0,
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

subscribe(Pair) ->
    gen_server:cast(?SERVER, {subscribe, string:lowercase(Pair)}).

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
    {ok, Connection} = gun:open("stream.binance.com", 9443, #{
                                  protocols => [http],
                                  transport => tls
                                 }),
    lager:info("Starting ~p", [?MODULE]),
    {ok, 
     #connection{
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
handle_cast({subscribe, Pair},
            #connection{
               connection = Connection,
               subscriptions = Subscribtions
              } = State) ->
    Subscribe = #{
      id => 1,
      method => <<"SUBSCRIBE">>,
      params => [ <<Pair/bytes, "@depth@100ms">> ]
     },
    gun:ws_send(Connection, {text, jsx:encode(Subscribe)}),
    {noreply, State#connection{subscriptions = [Pair | Subscribtions]}};
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
handle_info({gun_up, Connection, http}, #connection{connection = Connection} = State) ->
    lager:info("HTTP connected ~p", [Connection]),
    Ref = gun:ws_upgrade(Connection, "/ws"),
    {noreply, State#connection{ref = Ref}, ?HEATBREAT_TIMEOUT};
handle_info({gun_upgrade, Connection, Ref, _Protocols, _Headers},
            #connection{
               connection = Connection,
               subscriptions = Subscribtions
              } = State) ->
    lager:info("WS connected ~p", [Ref]),
    lists:foreach(fun subscribe/1, Subscribtions),
    {noreply, State#connection{ref = Ref}, ?HEATBREAT_TIMEOUT};
handle_info({gun_ws, Connection, Ref, {text, <<"[1010]">>}},
            #connection{connection = Connection, ref = Ref} = State) ->
    lager:debug("Heatbreat"),
    {noreply, State, ?HEATBREAT_TIMEOUT};
handle_info({gun_ws, Connection, Ref, {text, Data}},
            #connection{
               connection = Connection,
               ref = Ref
              } = State) ->
    lager:debug("Received data: ~p", [Data]),
    Json = jsx:decode(Data, [return_maps]),
    binance_pair_srv:update(Json),
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
