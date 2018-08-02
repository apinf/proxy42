-module(proxy42_router).
-behaviour(vegur_interface).
-include("api.hrl").
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         auth/2,
         rate_limit/3,
         backend_request_params/3,
         transform_response_headers/2,
         feature/2,
         additional_headers/4,
         error_page/4]).

-import(p42_req_ctx, [set_api/2, get_api/1,
                      set_developer_id/2,
                      set_user_id/2, get_servers/1,
                      set_tries/2, get_tries/1,
                      get_auth_info/1, get_rl_info/1,
                      get_log_fn/1,
                      get_outgoing_hostname/1,
                      apply_domain_settings/2,
                      apply_api_settings/2,
                      apply_sub_request_settings/2,
                      apply_developer_settings/2,
                      apply_app_user_settings/2,
                      get_api_id/1]).
-import(p42_req, [get_path/1, get_method/1, get_qs/1, get_headers/1]).
-import(p42_settings, [get_api_for/2,
                       get_settings_for_domain/1,
                       get_settings_for_api/1,
                       get_settings_for_developer/1,
                       get_settings_for_app_user/1,
                       get_settings_for_request/2]).

% Upstream is a cowboyku request.
init(_AcceptTime, Upstream) ->
  % RNGs require per-process seeding
  rand:seed(exs1024),
  % TODO:LOG: request init
  {ok, Upstream, p42_req_ctx:init()}.

lookup_domain_name(IncomingDomain, Upstream, State) ->
  DomainSettings = get_settings_for_domain(IncomingDomain),
  State1 = apply_domain_settings(DomainSettings, State),
  {Path, Req1} = get_path(Upstream),
  %% PathInfo is an array of binaries representing path components between /
  %% TODO: Get rid of API and only return APIId
  {ok, API} = get_api_for(IncomingDomain, Path),
  State2 = set_api(API, State1),

  APIId = get_api_id(State2),
  APISettings = get_settings_for_api(APIId),
  State3 = apply_api_settings(APISettings, State2),
  SReqSettings = get_settings_for_request(Upstream, State3),
  State4 = apply_sub_request_settings(SReqSettings, State3),
  {ok, API, Req1, State4}.

auth(Req, State) ->
  % {allow, Req, State}.
  % {deny, Req, State}.
  % {{rate_limit, "whoever"}, Req, State}.
  {PluginConfigId, AuthModule} = get_auth_info(State),
  Response0 = (catch AuthModule:auth(PluginConfigId, Req, State)),
  Response = case Response0 of
               {'EXIT', _Reason} -> deny;
               Val -> Val
             end,
  NewState = case Response of
               {DeveloperId, UserId} when is_binary(DeveloperId), is_binary(UserId) ->
                 S0 = set_developer_id(DeveloperId, State),
                 S1 = set_user_id(UserId, S0),
                 DevSettings = get_settings_for_developer(DeveloperId),
                 UserSettings = get_settings_for_app_user(UserId),
                 S2 = apply_developer_settings(DevSettings, S1),
                 S3 = apply_app_user_settings(UserSettings, S2),
                 S3;
               DeveloperId when is_binary(DeveloperId) ->
                 S0 = set_developer_id(DeveloperId, State),
                 DevSettings = get_settings_for_developer(DeveloperId),
                 S1 = apply_developer_settings(DevSettings, S0),
                 S1;
               deny -> State;
               ignore_rate_limit -> State
             end,
  {Response, Req, NewState}.

rate_limit(RLTag, _Req, State) ->
  % {allow, State}.
  % {{allow, Limit, Remaining, Reset}, State}.
  % {deny, State}.
  % {{deny, RetryAfter}, State}.
  {_ConfigId, RLModule} = get_rl_info(State),
  RateLimitResults = RLModule:check(RLTag, State),
  {RateLimitResults, State}.

checkout_service(_API, Upstream, State) ->
  Servers = get_servers(State),
  Tried = get_tries(State),
  Available = Servers -- Tried,
  case Available of
    [] ->
      {error, all_blocked, Upstream, State};
    _ ->
      N = rand:uniform(length(Available)),
      Pick = lists:nth(N, Available),
      NewState = set_tries([Pick | Tried], State),
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
  API = get_api(State),
  {Method, Req2} = get_method(Upstream),
  {OrigPath, Req3} = get_path(Req2),
  #api{
    hostname = Host,
    frontend_prefix = FP,
    backend_prefix = BP
  } = API,
  % Replace initial FP in incoming req path with BP
  Path = re:replace(OrigPath, ["^", FP], BP),
  Req4 = Req3,
  {Qs, Req5} = get_qs(Req4),
  {Headers, Req6} = get_headers(Req5),
  FullPath = case Qs of
               <<>> -> Path;
               _ -> [Path, "?", Qs]
             end,
  {Headers1, Req7} = vegur_proxy42_middleware:add_proxy_headers(Headers, Req6),
  Params = {Method, Headers1, Body, FullPath, Host},
  {Params, Req7, State}.

transform_response_headers(Headers, State) ->
  NewHost = get_outgoing_hostname(State),
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
error_page({upstream, _Reason}, _API, Upstream, HandlerState) ->
  %% Blame the caller
  {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _API, Upstream, HandlerState) ->
  %% Blame the server
  {{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _API, Upstream, HandlerState) ->
  %% Who knows who was to blame!
  {{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _API, Upstream, HandlerState) ->
  {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _API, Upstream, HandlerState) ->
  {{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _API, Upstream, HandlerState) ->
  {{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _API, Upstream, HandlerState) ->
  {{500, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
  ok.

