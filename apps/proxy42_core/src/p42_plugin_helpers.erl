-module(p42_plugin_helpers).
-compile([export_all]).
-include("domain_group.hrl").


get_rate_limit(ReqMeta) ->
  DomainGroup = get_api(ReqMeta),
  DomainGroup#domain_group.rate_limit.

get_api_id(ReqMeta) ->
  DomainGroup = get_api(ReqMeta),
  DomainGroup#domain_group.id.

get_api(ReqMeta) ->
  maps:get(domain_group, ReqMeta).
