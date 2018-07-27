-module(p42_req_ctx).
-compile([export_all]).
-include("domain_group.hrl").

-define(G(Key), maps:get(Key, ReqCtx)).
-define(S(Key,Val), maps:put(Key, Val, ReqCtx)).

init() ->
  %% TODO: Include global settings here
  #{
    tries => []
   }.

set_domain(Domain, ReqCtx) -> ?S(domain, Domain).
get_domain(ReqCtx) -> ?G(domain).

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
  API#domain_group.servers.

get_outgoing_hostname(_ReqCtx) ->
  todo. % TODO

get_rate_limit(ReqCtx) ->
  API = get_api(ReqCtx),
  API#domain_group.rate_limit.

get_api_id(ReqCtx) ->
  DomainGroup = get_api(ReqCtx),
  DomainGroup#domain_group.id.

get_auth_info(_ReqCtx) ->
  %% TODO: do this properly
  %% TODO: Get authmodule dynamically
  %% alias/ConfigId, module
  {<<"bearer-auth">>, p42_auth_key}.

get_rl_info(_ReqCtx) ->
  {<<"rl_default">>, p42_rl_ftbucket}.
 %% TODO: get RLModule this dynamically

get_lb_info(ReqCtx) ->
  todo.
get_log_fn(ReqCtx) ->
  todo.
get_request_transforms(ReqCtx) ->
  todo.
add_req_transform(Transform, ReqCtx) ->
  todo.
get_resp_transforms(ReqCtx) ->
  todo.
add_resp_transform(Transform, ReqCtx) ->
  todo.
