-module(proxy42_config).
-include("domain_group.hrl").
-export([domain_config/2]).

%% @doc
%% Find the domain config based on incoming domain and path.
domain_config(Domain, Path) ->
    [<<"">>, Prefix, _] = re:split(Path, "/", [{return, binary},{parts, 3}]),
    % TODO: Fix this to consider incoming domain.
    %    This requires adding incoming domain to domain group record.
    DG = storage_app:find_domain_group(#domain_group{frontend_prefix = <<"/", Prefix/binary, "/">>, _ = '_'}),
  % DomainGroup can be an arbitrary datastructure.
  % This gets passed to checkout_service,
  %   which returns a service, again an arbitrary data structure
  % The service gets passed to service_backend which should return
  % domain name/ip and port.
  {ok, DG}.
