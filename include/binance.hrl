% General defaults
-define(HEATBREAT_TIMEOUT, 2000).
-define(HOST, "api.binance.com").
%
% API endpoints
% Public
-define(TICKER, "/api/v3/ticker/24hr").
-define(ORDER_BOOK, "/api/v3/depth").
-define(TRADE_HISTORY, "/api/v3/trades").
-define(CHART_DATA, "/api/v3/klines").
-define(CURRENCIES, "/api/v3/exchangeInfo").

% Private
-define(BALANCES, "/sapi/v1/capital/config/getall").
-define(OPEN_ORDERS, "/api/v3/openOrders").
-define(ORDER_STATUS, "/api/v3/order").
-define(BUY, "/api/v3/order").
-define(SELL, "/api/v3/order").


-record(connection, {
          connection :: pid() | undefined,
          headers = [] :: gun:req_headers(),
          key = <<>> :: binary(),
          secret = <<>> :: binary(),
          from :: pid() | undefined,
          ref :: reference() | undefined,
          subscriptions = sets:new() :: sets:set(binary())
         }).

-record(pair, {
          code :: pos_integer() | undefined,
          pair :: binary() | undefined,
          pid :: pid() | undefined
         }).

