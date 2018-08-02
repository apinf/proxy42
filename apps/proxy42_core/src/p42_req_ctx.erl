-module(p42_req_ctx).
-compile([export_all]).
-include("api.hrl").

-define(G(Key), maps:get(Key, ReqCtx)).
-define(G(Key, Default), maps:get(Key, ReqCtx, Default)).
-define(S(Key,Val), maps:put(Key, Val, ReqCtx)).

init(ReqId, IncomingPeer) ->
  %% TODO: Include global settings here
  #{tries => []
   ,request_id => ReqId
   ,phase => init
   ,incoming_peer => IncomingPeer
   %% ,log_fn => fun noop/2
   ,log_fn => fun console_log/2
   }.

apply_domain_settings(DomSettings, ReqCtx) ->
  %% TODO
  ReqCtx.
apply_api_settings(APISettings, ReqCtx) ->
  %% TODO
  ReqCtx.
apply_sub_request_settings(SubReqSettings, ReqCtx) ->
  %% TODO
  ReqCtx.
apply_developer_settings(DevSettings, ReqCtx) ->
  %% TODO
  ReqCtx.
apply_app_user_settings(AppUserSettings, ReqCtx) ->
  %% TODO
  ReqCtx.

set_domain(Domain, ReqCtx) -> ?S(domain, Domain).
get_domain(ReqCtx) -> ?G(domain).

set_phase(Phase, ReqCtx) -> ?S(phase, Phase).
get_phase(ReqCtx) -> ?G(phase).

set_api(API, ReqCtx) -> ?S(api, API).
get_api(ReqCtx) -> ?G(api).

set_developer_id(DeveloperId, ReqCtx) -> ?S(developer_id, DeveloperId).
get_developer_id(ReqCtx) -> ?G(developer_id).

set_user_id(UserId, ReqCtx) -> ?S(user_id, UserId).
get_user_id(ReqCtx) -> ?G(user_id).

set_tries(Tries, ReqCtx) -> ?S(tries, Tries).
get_tries(ReqCtx) -> ?G(tries).

get_servers(ReqCtx) ->
  API = get_api(ReqCtx),
  API#api.servers.

get_outgoing_hostname(_ReqCtx) ->
  todo. % TODO

get_rate_limit(ReqCtx) ->
  API = get_api(ReqCtx),
  API#api.rate_limit.

get_api_id(ReqCtx) ->
  API = get_api(ReqCtx),
  API#api.id.

get_auth_info(_ReqCtx) ->
  %% TODO: do this properly
  %% TODO: Get authmodule dynamically
  %% alias/ConfigId, module
  {<<"bearer-auth">>, p42_auth_key}.

get_rl_info(_ReqCtx) ->
  {<<"rl_default">>, p42_rl_ftbucket}.
 %% TODO: get RLModule this dynamically

get_resp_status(ReqCtx) -> ?G(resp_status).
set_resp_status(RespStatus, ReqCtx) -> ?S(resp_status, RespStatus).

set_response_code(Code, ReqCtx) ->
   ?S(response_code, Code).
get_response_code(ReqCtx) ->
  ?G(response_code).

get_lb_info(ReqCtx) ->
  todo.

get_log_fn(ReqCtx) ->
  ?G(log_fn).

get_request_transforms(ReqCtx) ->
  todo.
add_req_transform(Transform, ReqCtx) ->
  todo.
get_resp_transforms(ReqCtx) ->
  todo.
add_resp_transform(Transform, ReqCtx) ->
  todo.

noop(_,_) -> ok.

%% TODO: Move elsewhere
console_log(LogInfo, ReqCtx) ->
  io:format("~p~n~n~p~n======~n", [LogInfo, ReqCtx]).
