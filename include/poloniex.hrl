% General defaults
-define(HEATBREAT_TIMEOUT, 2000).
-define(HOST, "poloniex.com").
%
% API Paths
-define(PUBLIC_PATH, "/public").
-define(PRIVATE_PATH, "/tradingApi").

% API endpoints
% Public
-define(TICKER, "returnTicker").
-define(VOL24, "return24hVolume").
-define(ORDER_BOOK, "returnOrderBook").
-define(TRADE_HISTORY, "returnTradeHistory").
-define(CHART_DATA, "returnChartData").
-define(CURRENCIES, "returnCurrencies").

% Private
-define(BALANCES, "returnCompleteBalances").
-define(OPEN_ORDERS, "returnOpenOrders").
-define(ORDER_STATUS, "returnOrderStatus").
-define(BUY, "buy").
-define(SELL, "sell").


-record(connection, {
          connection :: pid() | undefined,
          headers = [] :: gun:req_headers(),
          key = <<>> :: binary(),
          secret = <<>> :: binary(),
          from :: pid() | undefined,
          ref :: reference() | undefined
         }).

-record(pair, {
          code :: pos_integer() | undefined,
          pair :: binary() | undefined,
          pid :: pid() | undefined
         }).

