-define(HEATBREAT_TIMEOUT, 2000).

-record(connection, {
          connection,
          ref
         }).

-record(pair, {
          code :: pos_integer() | undefined,
          pair :: binary() | undefined,
          pid :: pid() | undefined
         }).

