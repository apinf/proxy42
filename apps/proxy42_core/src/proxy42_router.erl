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
                      get_log_strategy/1,
                      get_outgoing_hostname/1,
                      set_resp_status/2,
                      set_domain/2,
                      apply_domain_settings/2,
                      apply_api_settings/2,
                      apply_sub_request_settings/2,
                      apply_developer_settings/2,
                      apply_app_user_settings/2,
                      get_api_id/1]).
-import(p42_req, [get_path/1, get_method/1, get_qs/1, get_headers/1,
                  get_logging/1]).
-import(p42_settings, [get_api_for/2,
                       get_settings_for_domain/1,
                       get_settings_for_api/1,
                       get_settings_for_developer/1,
                       get_settings_for_app_user/1,
                       get_settings_for_request/2]).

-type router_state() :: p42_req_ctx:req_ctx().
-type req() :: p42_req:req().
-type req_params() :: {p42_req:method(),
                       p42_req:headers(),
                       p42_req:body(),
                       p42_req:path(),
                       cowboyku_req:host()}.
-type auth_response() :: {p42_req_ctx:developer_id(), p42_req_ctx:user_id()}
                        | p42_req_ctx:developer_id()
                        | deny
                        | ignore_rate_limit.
-type rltag() :: atom().
-type rl_results() :: {allow, non_neg_integer(), non_neg_integer(), non_neg_integer()}
                    | {deny, non_neg_integer()}.
-export_type([rltag/0, rl_results/0]).

-spec init(erlang:timestamp(), req()) ->
              {ok, req(), router_state()}.
init(_AcceptTime, Upstream) ->
  % RNGs require per-process seeding
  rand:seed(exs1024),
  {ok, Upstream, p42_req_ctx:init(Upstream)}.

-spec lookup_domain_name(hostname(), req(), router_state()) ->
                            {ok, api(), req(), router_state()}.
lookup_domain_name(IncomingDomain, Upstream, State) ->
  DomainSettings = get_settings_for_domain(IncomingDomain),
  State1 = apply_domain_settings(DomainSettings, State),
  State2 = set_domain(IncomingDomain, State1),
  Path = get_path(Upstream),
  %% PathInfo is an array of binaries representing path components between /
  %% TODO: Get rid of API and only return APIId
  {ok, API} = get_api_for(IncomingDomain, Path),
  State3 = set_api(API, State2),

  APIId = get_api_id(State3),
  APISettings = get_settings_for_api(APIId),
  State4 = apply_api_settings(APISettings, State3),
  SReqSettings = get_settings_for_request(Upstream, State4),
  State5 = apply_sub_request_settings(SReqSettings, State4),
  {ok, API, Upstream, State5}.

%%
%% called by proxy42_authenticate_middleware
%% while running request authentication
-spec auth(req(), router_state()) -> {auth_response(),
                                      req(),
                                      router_state()}.
auth(Req, State) ->
  % {allow, Req, State}.
  % {deny, Req, State}.
  % {{rate_limit, "whoever"}, Req, State}.
  {PluginConfigId, AuthModule} = get_auth_info(State),
  Response0 = (catch AuthModule:auth(PluginConfigId, Req, State)),
  Response = case Response0 of
               {'EXIT', _} = Error  ->
                error_logger:info_report([Error]),
                deny;
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

-spec rate_limit(rltag(), req(), router_state()) -> {rl_results(), router_state()}.
rate_limit(RLTag, _Req, State) ->
  % {allow, State}.
  % {{allow, Limit, Remaining, Reset}, State}.
  % {deny, State}.
  % {{deny, RetryAfter}, State}.
  {_ConfigId, RLModule} = get_rl_info(State),
  RateLimitResults = RLModule:check(RLTag, State),
  {RateLimitResults, State}.

-spec checkout_service(api(), req(), router_state()) ->
                          {error, all_blocked, req(), router_state()} |
                          {service, binary(), req(), router_state()}.
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

-spec service_backend({http, inet:ip_address() | inet:hostname(), inet:port_number()},
                      req(), router_state()) ->
                         {{inet:ip_address() | inet:hostname(), inet:port_number()},
                          req(), router_state()}.
service_backend({http, IPorDomain, Port}, Upstream, State) ->
  %% extract the IP:PORT from the chosen server.
  %% To enable keep-alive, use:
  %% `{{keepalive, {default, {IP,Port}}}, Upstream, State}'
  %% To force the use of a new keepalive connection, use:
  %% `{{keepalive, {new, {IP,Port}}}, Upstream, State}'
  %% Otherwise, no keepalive is done to the back-end:
  {{IPorDomain, Port}, Upstream, State}.

-spec backend_request_params(p42_req:body(), req(), router_state()) ->
                                {req_params(), req(), router_state()}.
backend_request_params(Body, Upstream, State) ->
  API = get_api(State),
  Method = get_method(Upstream),
  OrigPath = get_path(Upstream),
  #api{
    hostname = Host,
    frontend_prefix = FP,
    backend_prefix = BP
  } = API,
  % Replace initial FP in incoming req path with BP
  Path = re:replace(OrigPath, ["^", FP], BP),
  Qs = get_qs(Upstream),
  Headers = get_headers(Upstream),
  FullPath = case Qs of
               <<>> -> Path;
               _ -> [Path, "?", Qs]
             end,
  {Headers1, Req} = vegur_proxy42_middleware:add_proxy_headers(Headers, Upstream),
  Params = {Method, Headers1, Body, FullPath, Host},
  {Params, Req, State}.

transform_response_headers(Headers, State) ->
  NewHost = get_outgoing_hostname(State),
  NewHeaders = lists:keyreplace(<<"host">>, 1, Headers, {<<"host">>, NewHost}),
  {NewHeaders, State}.

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

terminate(RespStatus, Upstream, State) ->
  State1 = set_resp_status(RespStatus, State),
  State2 = p42_req_ctx:set_response_code(p42_req:get_response_code(Upstream), State1),
  LogInfo = get_logging(Upstream),
  dispatch_logs(LogInfo, State2),
  ok.

dispatch_logs(LogInfo, State) ->
  LogMod = get_log_strategy(State),
  LogMod:log(LogInfo, State).
