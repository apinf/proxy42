-module(proxy42_config).
-export([domain_config/2]).

%% @doc Find the 
domain_config(_Domain, PathInfo) ->
  DomainGroup = [{{127,0,0,1}, 8080}, {"localhost", 8080}],
  % DomainGroup can be an arbitrary datastructure.
  % This gets passed to checkout_service,
  %   which returns a service, again an arbitrary data structure
  % The service gets passed to service_backend which should return
  % domain name/ip and port.
  TransformedPath = PathInfo,
  {ok, DomainGroup, TransformedPath}.
