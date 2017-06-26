-module(proxy42_router).
-behaviour(vegur_interface).
-include("domain_group.hrl").
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         auth_config/2,
         auth/3,
         rate_limit/3,
         backend_request_params/3,
         transform_response_headers/2,
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
  {Path, Req1} = cowboyku_req:path(Upstream),
  % PathInfo is an array of binaries representing path components between /
  {ok, DomainGroup} = proxy42_config:domain_config(IncomingDomain, Path),
  NewState = maps:put(domain_group, DomainGroup, State),
  {ok, DomainGroup, Req1, NewState}.

checkout_service(DomainGroup = #domain_group{strategy = random}, Upstream, State = #{ tries := Tried }) ->
  #domain_group{servers = Servers} = DomainGroup,
  Available = Servers -- Tried,
  case Available of
    [] ->
      {error, all_blocked, Upstream, State};
    _ ->
      N = rand:uniform(length(Available)),
      Pick = lists:nth(N, Available),
      NewState = maps:put(tries, [Pick | Tried], State),
      {service, Pick, Upstream, NewState}
  end.

service_backend({http, IPorDomain, Port}, Upstream, State) ->
  %% extract the IP:PORT from the chosen server.
  %% To enable keep-alive, use:
  %% `{{keepalive, {default, {IP,Port}}}, Upstream, State}'
  %% To force the use of a new keepalive connection, use:
  %% `{{keepalive, {new, {IP,Port}}}, Upstream, State}'
  %% Otherwise, no keepalive is done to the back-end:
  {{IPorDomain, Port}, Upstream, State}.

backend_request_params(Body, Upstream, State) ->
  DomainGroup = maps:get(domain_group, State),
  {Method, Req2} = cowboyku_req:method(Upstream),
  {OrigPath, Req3} = cowboyku_req:path(Req2),
  #domain_group{
    hostname = Host,
    frontend_prefix = FP,
    backend_prefix = BP
  } = DomainGroup,
  % Replace initial FP in incoming req path with BP
  Path = re:replace(OrigPath, ["^", FP], BP),
  Req4 = Req3,
  {Qs, Req5} = cowboyku_req:qs(Req4),
  {Headers, Req6} = cowboyku_req:headers(Req5),
  FullPath = case Qs of
               <<>> -> Path;
               _ -> [Path, "?", Qs]
             end,
  {Headers1, Req7} = vegur_proxy42_middleware:add_proxy_headers(Headers, Req6),
  Params = {Method, Headers1, Body, FullPath, Host},
  erlang:display(Params),
  {Params, Req7, State}.

auth_config(Req, State) ->
  Config = [
   {header, <<"proxy-authorization">>, strip}
  ,{header, <<"authorization">>, strip}
  ],
  {Config, Req, State}.

auth(AuthInfo, Req, State) ->
  % {allow, Req, State}.
  % {deny, Req, State}.
  {{rate_limit, "whoever"}, Req, State}.

rate_limit(User, Req, State) ->
  % {allow, State}.
  % {deny, State}.
  % {{deny, 120}, State}.
  {{allow, 100, 99, 120}, State}.

transform_response_headers(Headers, State = #{domain_group := DG}) ->
  #domain_group{hostname = NewHost} = DG,
  lists:keyreplace(<<"host">>, 1, Headers, {<<"host">>, NewHost}),
  {Headers, State}.

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
