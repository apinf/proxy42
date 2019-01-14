
-module(p42_req_ctx).
-compile([export_all]).
-include("api.hrl").

-import(p42_req, [get_req_id/1, incoming_peer/1,
                  incoming_ip/1, get_path/1,
                  get_method/1]).

-define(G(Key), maps:get(Key, ReqCtx)).
-define(G(Key, Default), maps:get(Key, ReqCtx, Default)).
-define(S(Key,Val), maps:put(Key, Val, ReqCtx)).

-type dom_settings() :: any().
-type api_settings() :: any().
-type sub_req_settings() :: any().
-type dev_settings() :: any().
-type user_settings() :: any().
-type auth_info() :: {binary(), atom()}.
-type rl_info() :: {binary(), atom()}.
-type log_strategy() :: atom().
-type developer_id() :: binary() | undefined.
-type user_id() :: binary() | undefined.
-type req_ctx() ::
#{
  request_id  => p42_req:req_id(),
  incoming_peer => p42_req:peer(),
  phase => atom(),
  tries => [binary()],
  path => p42_req:path(),
  method => p42_req:method(),
  log_strategy => atom(),
  developer_id => developer_id(),
  user_id => user_id(),
  _ => _
 }.
-export_type([req_ctx/0, developer_id/0, user_id/0]).

-spec init(p42_req:req()) -> req_ctx().
init(Req) ->
  %% TODO: Include global settings here
  ReqId = get_req_id(Req),
  IncomingPeer = incoming_peer(Req),
  Path = get_path(Req),
  Method = get_method(Req),
  #{tries => []
   ,request_id => ReqId
   ,phase => init
   ,incoming_peer => IncomingPeer
   ,path => Path
   ,method => Method
   ,developer_id => undefined
   ,user_id => undefined
   ,log_strategy => p42_log_plugin_es
   }.

-spec apply_domain_settings(dom_settings(), req_ctx()) -> req_ctx().
apply_domain_settings(_DomSettings, ReqCtx) ->
  %% TODO
  ReqCtx.
-spec apply_api_settings(api_settings(), req_ctx()) -> req_ctx().
apply_api_settings(_APISettings, ReqCtx) ->
  %% TODO
  ReqCtx.
-spec apply_sub_request_settings(sub_req_settings(), req_ctx()) -> req_ctx().
apply_sub_request_settings(_SubReqSettings, ReqCtx) ->
  %% TODO
  ReqCtx.
-spec apply_developer_settings(dev_settings(), req_ctx()) -> req_ctx().
apply_developer_settings(_DevSettings, ReqCtx) ->
  %% TODO
  ReqCtx.
-spec apply_app_user_settings(user_settings(), req_ctx()) -> req_ctx().
apply_app_user_settings(_AppUserSettings, ReqCtx) ->
  %% TODO
  ReqCtx.

-spec ctx_set(atom(), term(), req_ctx()) -> req_ctx().
ctx_set(What, Val, ReqCtx) -> ?S(What, Val).
-spec ctx_get(atom(), req_ctx()) -> term().
ctx_get(incoming_ip, ReqCtx) ->
  {Addr, _Port} = ctx_get(incoming_peer, ReqCtx),
  erlang:list_to_binary(inet:ntoa(Addr));
ctx_get(api_id, ReqCtx) ->
  get_api_id(ReqCtx);
ctx_get(What, ReqCtx) -> ?G(What).

-spec set_domain(cowboyku_req:host(), req_ctx()) -> req_ctx().
set_domain(Domain, ReqCtx) -> ?S(domain, Domain).
-spec get_domain(req_ctx()) -> cowboyku_req:host().
get_domain(ReqCtx) -> ?G(domain).

-spec set_phase(atom(), req_ctx()) -> req_ctx().
set_phase(Phase, ReqCtx) -> ?S(phase, Phase).
-spec get_phase(req_ctx()) -> atom().
get_phase(ReqCtx) -> ?G(phase).

-spec set_api(api(), req_ctx()) -> req_ctx().
set_api(API, ReqCtx) -> ?S(api, API).
-spec get_api(req_ctx()) -> api().
get_api(ReqCtx) -> ?G(api).

-spec set_developer_id(binary(), req_ctx()) -> req_ctx().
set_developer_id(DeveloperId, ReqCtx) -> ?S(developer_id, DeveloperId).
-spec get_developer_id(req_ctx()) -> binary() | undefined.
get_developer_id(ReqCtx) -> ?G(developer_id).

-spec set_user_id(binary(), req_ctx()) -> req_ctx().
set_user_id(UserId, ReqCtx) -> ?S(user_id, UserId).
-spec get_user_id(req_ctx()) -> binary() | undefined.
get_user_id(ReqCtx) -> ?G(user_id).

-spec set_tries(tries(), req_ctx()) -> req_ctx().
set_tries(Tries, ReqCtx) -> ?S(tries, Tries).
-spec get_tries(req_ctx()) -> tries().
get_tries(ReqCtx) -> ?G(tries).

-spec get_servers(req_ctx()) -> servers().
get_servers(ReqCtx) ->
  API = get_api(ReqCtx),
  API#api.servers.

-spec get_outgoing_hostname(req_ctx()) -> hostname().
get_outgoing_hostname(_ReqCtx) ->
  todo. % TODO

-spec get_rate_limit(req_ctx()) -> rate_limit().
get_rate_limit(ReqCtx) ->
  API = get_api(ReqCtx),
  API#api.rate_limit.

-spec get_api_id(req_ctx()) -> api_id().
get_api_id(ReqCtx) ->
  API = get_api(ReqCtx),
  API#api.id.

-spec get_auth_info(req_ctx()) -> auth_info().
get_auth_info(ReqCtx) ->
  %% alias/ConfigId, module
  %%{<<"bearer-auth">>, p42_auth_key}.
  API = get_api(ReqCtx),
  AuthConfig = API#api.auth_config,
  {AuthStrategy, ConfigId} = AuthConfig,
  %% AuthModule = get_module_for_auth_strategy(AuthStrategy),
  AuthModule=AuthStrategy,
  {ConfigId, AuthModule}.

%% TODO: cleanup
get_module_for_auth_strategy(S) ->
  AuthRegistry = 'Elixir.Proxy42.ControlApi.Apis':registered_auth_strategies(),
  maps:find(S, AuthRegistry).

-spec get_rl_info(req_ctx()) -> rl_info().
get_rl_info(_ReqCtx) ->
  {<<"rl_default">>, p42_rl_ftbucket}.
 %% TODO: get RLModule this dynamically

-spec set_resp_status(binary(), req_ctx()) -> req_ctx().
set_resp_status(RespStatus, ReqCtx) -> ?S(resp_status, RespStatus).
-spec get_resp_status(req_ctx()) -> binary().
get_resp_status(ReqCtx) -> ?G(resp_status).

-spec set_response_code(cowboyku:http_status(), req_ctx()) -> req_ctx().
set_response_code(Code, ReqCtx) ->
   ?S(response_code, Code).
-spec get_response_code(req_ctx()) -> cowboyku:http_status().
get_response_code(ReqCtx) ->
  ?G(response_code).

get_lb_info(_ReqCtx) ->
  todo.

-spec get_log_strategy(req_ctx()) -> log_strategy().
get_log_strategy(ReqCtx) ->
  ?G(log_strategy).

get_request_transforms(_ReqCtx) ->
  todo.
add_req_transform(_Transform, _ReqCtx) ->
  todo.
get_resp_transforms(_ReqCtx) ->
  todo.
add_resp_transform(_Transform, _ReqCtx) ->
  todo.

-spec noop(term(), term()) -> ok.
noop(_,_) -> ok.

%% TODO: Move elsewhere
console_log(LogInfo, ReqCtx) ->
  io:format("~p~n~n~p~n======~n", [LogInfo, ReqCtx]).
