-define(HEATBREAT_TIMEOUT, 2000).
-define(HOST, "poloniex.com").
-define(PUBLIC_PATH, "/public").
-define(TICKER, "returnTicker").
-define(VOL24, "return24hVolume").
-define(ORDER_BOOK, "returnOrderBook").
-define(TRADE_HISTORY, "returnTradeHistory").
-define(CHART_DATA, "returnChartData").
-define(CURRENCIES, "returnCurrencies").


-record(connection, {
          connection :: pid() | undefined,
          from :: pid() | undefined,
          ref :: reference() | undefined
         }).

-record(pair, {
          code :: pos_integer() | undefined,
          pair :: binary() | undefined,
          pid :: pid() | undefined
         }).

