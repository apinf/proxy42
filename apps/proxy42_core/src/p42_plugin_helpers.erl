-module(p42_plugin_helpers).
-compile([export_all]).
-include("domain_group.hrl").


get_rate_limit(DomainGroup) ->
  DomainGroup#domain_group.rate_limit.

get_api_id(DomainGroup) ->
  DomainGroup#domain_group.id.
