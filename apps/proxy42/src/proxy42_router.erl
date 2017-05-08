-module(proxy42_router).
-behaviour(vegur_interface).
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         additional_headers/4,
         error_page/4]).

state() ->
  #{
   tries => []
  }.

% Upstream is a cowboyku request.
init(_AcceptTime, Upstream) ->
  % RNGs require per-process seeding
  rand:seed(exs1024),
  % TODO:LOG: request init
  {ok, Upstream, state()}.

lookup_domain_name(IncomingDomain, Upstream, State) ->
  {PathInfo, Req1} = cowboyku_req:path_info(Upstream),
  % PathInfo is an array of binaries representing path components between /
  {ok, DomainGroup} = proxy42_config:domain_config(IncomingDomain, PathInfo),
  {ok, DomainGroup, Req1, State}.

checkout_service(DomainGroup, Upstream, State = #{ tries := Tried }) ->
  Available = DomainGroup -- Tried,
  case Available of
    [] ->
      {error, all_blocked, Upstream, State};
    _ ->
      N = rand:uniform(length(Available)),
      Pick = lists:nth(N, Available),
      NewState = maps:put(tries, [Pick | Tried], State),
      {service, Pick, Upstream, NewState}
  end.

service_backend({IP, Port}, Upstream, State) ->
  %% extract the IP:PORT from the chosen server.
  %% To enable keep-alive, use:
  %% `{{keepalive, {default, {IP,Port}}}, Upstream, State}'
  %% To force the use of a new keepalive connection, use:
  %% `{{keepalive, {new, {IP,Port}}}, Upstream, State}'
  %% Otherwise, no keepalive is done to the back-end:
  {{IP, Port}, Upstream, State}.

backend_request_params(Body, Upstream, State) ->
  {Method, Req2} = cowboyku_req:method(Upstream),
  {Path, Req3} = cowboyku_req:path(Req2),
  {Host, Req4} = cowboyku_req:host(Req3),
  {Qs, Req5} = cowboyku_req:qs(Req4),
  {Headers, Req6} = cowboyku_req:headers(Req5),
  FullPath = case Qs of
               <<>> -> Path;
               _ -> <<Path/binary, "?", Qs/binary>>
             end,
  {Headers2, Req7} = vegur_proxy42_middleware:add_proxy_headers(Headers, Req6),
  Params = {Method, Headers2, Body, FullPath, Host},
  {Params, Req7, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
  %% if we tracked total connections, we would decrement the counters here
  {ok, Upstream, State}.

feature(_WhoCares, State) ->
  {disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
  {[], State}.


%% Vegur-returned errors that should be handled no matter what.
%% Full list in src/vegur_stub.erl
error_page({upstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
  %% Blame the caller
  {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
  %% Blame the server
  {{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _DomainGroup, Upstream, HandlerState) ->
  %% Who knows who was to blame!
  {{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
  {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
  {{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
  {{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _DomainGroup, Upstream, HandlerState) ->
  {{500, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
  ok.
