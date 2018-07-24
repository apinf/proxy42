-module(p42_req_ctx).
-compile([export_all]).
-include("domain_group.hrl").


get_rate_limit(ReqCtx) ->
  DomainGroup = get_api(ReqCtx),
  DomainGroup#domain_group.rate_limit.

get_api_id(ReqCtx) ->
  DomainGroup = get_api(ReqCtx),
  DomainGroup#domain_group.id.

get_api(ReqCtx) ->
  maps:get(domain_group, ReqCtx).
