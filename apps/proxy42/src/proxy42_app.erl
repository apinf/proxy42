%%%-------------------------------------------------------------------
%% @doc myapp public API
%% @end
%%%-------------------------------------------------------------------

-module(proxy42_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  vegur:start_http(8080,
                   proxy42_router,
                   [{middlewares 
										,vegur_validate_headers
										,vegur_lookup_domain_middleware
										,vegur_continue_middleware
										,vegur_upgrade_middleware
										,vegur_lookup_service_middleware
										,vegur_proxy42_middleware
										}]
									),

  proxy42_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
