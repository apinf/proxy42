-type api_id() :: binary().
-type hostname() :: binary().
-type frontend_prefix() :: binary().
-type backend_prefix() :: binary().
-type servers() :: [binary()].
-type tries() :: servers().
-type lb_strategy() :: atom().
-type additional_headers() :: any().
-type rate_limit() :: non_neg_integer().
-type auth_strategy() :: atom().
-type auth_config() :: {auth_strategy, map()}.
-record(api, {
          id :: api_id() | '_',
          hostname :: hostname() | '_',
          frontend_prefix :: frontend_prefix() | '_',
          backend_prefix :: backend_prefix() | '_',
          servers :: servers() | '_',
          strategy :: lb_strategy() | '_',
          additional_headers :: additional_headers() | '_',
          rate_limit :: rate_limit() | '_',
          auth_config :: auth_config() | '_'}).
-type api() :: #api{}.
-export_type([api/0]).
