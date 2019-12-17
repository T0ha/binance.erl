%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-18 13:25:35.910979
%%%-------------------------------------------------------------------
-module(binance_http_public).

-behaviour(gen_server).
-include("binance.hrl").

%% API
-export([
         start_link/0,
         ticker/0,
         vol24/0,
         order_book/1, order_book/2,
         trade_history/1, trade_history/3,
         chart_data/4,
         currencies/0
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

ticker() ->
    gen_server:call(?SERVER, {get, ?TICKER, []}).

vol24() ->
    gen_server:call(?SERVER, {get, ?VOL24, []}).

order_book(Pair) ->
    order_book(Pair, 50).

order_book(Pair, Depth) when is_integer(Depth) ->
    order_book(Pair, integer_to_binary(Depth));
order_book(Pair, Depth) ->
    gen_server:call(?SERVER, {get, ?ORDER_BOOK, [
                                                 {"symbol", Pair},
                                                 {"limit", Depth}
                                                ]}).

trade_history(Pair) ->
    gen_server:call(?SERVER, {get, ?TRADE_HISTORY, [
                                                    {"symbol", Pair}
                                                   ]}).
trade_history(Pair, Start, End) when is_integer(Start) ->
    trade_history(Pair, integer_to_binary(Start), End);
trade_history(Pair, Start, End) when is_integer(End) ->
    trade_history(Pair, Start, integer_to_binary(End));
trade_history(Pair, Start, End) ->
    gen_server:call(?SERVER, {get, ?TRADE_HISTORY, [
                                                    {"symbol", Pair},
                                                    {"start", Start},
                                                    {"end", End}
                                                   ]}).

chart_data(Pair, Period, Start, End) when is_integer(Period) ->
    chart_data(Pair, integer_to_binary(Period), Start, End);
chart_data(Pair, Period, Start, End) when is_integer(Start) ->
    chart_data(Pair, Period, integer_to_binary(Start), End);
chart_data(Pair, Period, Start, End) when is_integer(End) ->
    chart_data(Pair, Period, Start, integer_to_binary(End));
chart_data(Pair, Period, Start, End) ->
    gen_server:call(?SERVER, {get, ?CHART_DATA, [
                                                    {"symbol", Pair},
                                                    {"period", Period},
                                                    {"start", Start},
                                                    {"end", End}
                                                   ]}).
currencies() ->
    gen_server:call(?SERVER, {get, ?CURRENCIES, []}).

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
    {ok, Connection} = connect(),
    lager:info("Starting ~p", [?MODULE]),
    {ok, 
     #connection{
            connection = Connection
           }}.

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
handle_call({get, Endpoint, Params}, _From, #connection{connection = Connection} = State) ->
    Ref = gun:get(Connection, api_url(Endpoint, Params)),
    {response, nofin, 200, _Headers} = gun:await(Connection, Ref),
    {ok, Data} = gun:await_body(Connection, Ref, 300000),
    Json = jsx:decode(Data),
    {reply, Json, State};
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
handle_info({gun_up, Connection, http}, #connection{connection = Connection} = State) ->
    lager:info("HTTP connected ~p", [Connection]),
    {noreply, State};
handle_info({gun_down, Connection, http, Reason, _KilledStreams},
            #connection{connection = Connection} = State) ->
    lager:info("Publick HTTP ~p disconnected with reason ~p, reconnecting", [Connection, Reason]),
    %{ok, NewConnection} = connect(),
    {noreply, State#connection{connection = Connection}};
handle_info({gun_responce, Connection, Ref, fin, Status, Headers},
            #connection{
               connection = Connection,
               ref = Ref,
               from = Pid
              } = State) ->
    lager:info("Empty HTTP responce recieved for ~p", [Ref]),
    gen_server:reply(Pid, <<>>),
    {noreply, State#connection{from = undefined, ref = undefined}};
handle_info({gun_responce, Connection, Ref, nofin, Status, Headers},
            #connection{
               connection = Connection,
               ref = Ref,
               from = Pid
              } = State) ->
    lager:info("HTTP responce recieved for ~p", [Ref]),
    {noreply, State};
handle_info({gun_data, Connection, Ref, nofin, Status, Headers},
            #connection{
               connection = Connection,
               ref = Ref,
               from = Pid
              } = State) ->
    lager:info("HTTP responce recieved for ~p", [Ref]),
    {noreply, State};
handle_info(_Info, State) ->
    lager:warning("Wrong message: ~p in module ~p state ~p", [_Info, ?MODULE, State]),
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
connect() ->
    lager:info("Connecting to ~s", [?HOST]),
    gun:open(?HOST, 443, #{protocols => [http]}).

api_url(Endpoint) ->
    api_url(Endpoint, []).

api_url(Endpoint, Params) ->
    Query = uri_string:compose_query(Params),
    Endpoint ++ "?" ++ Query.
