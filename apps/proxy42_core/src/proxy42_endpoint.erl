-module(proxy42_endpoint).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2]).

-define(REF, ?MODULE).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    gen_server:stop({local, ?MODULE}).

init([Port]) ->
    {ok, Pid} = vegur:start_http(Port,
                                 proxy42_router,
                                 [{ref, ?REF}
                                 ,{middlewares,
                                   [vegur_validate_headers
                                   ,vegur_lookup_domain_middleware
                                   ,proxy42_authenticate_middleware
                                   ,vegur_continue_middleware
                                   ,vegur_upgrade_middleware
                                   ,vegur_lookup_service_middleware
                                   ,vegur_proxy42_middleware
                                   ]
                                  }]
                                ),
    true = erlang:link(Pid),
    {ok, Pid, hibernate}.

terminate(_Reason, _State) ->
    ok = vegur:stop_http(?REF).
