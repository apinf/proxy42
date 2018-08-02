-module(p42_settings).
-export([get_api_for/2]).
-include("api.hrl").

%% @doc
%% Find the domain config based on incoming domain and path.
get_api_for(Domain, Path) ->
    [<<"">>, Prefix, _] = re:split(Path, "/", [{return, binary},{parts, 3}]),
    % TODO: Fix this to consider incoming domain.
    %    This requires adding incoming domain to api record.
    API = proxy42_storage:find_api(#api{frontend_prefix = <<"/", Prefix/binary, "/">>, _ = '_'}),
  % API can be an arbitrary datastructure.
  % This gets passed to checkout_service,
  %   which returns a service, again an arbitrary data structure
  % The service gets passed to service_backend which should return
  % domain name/ip and port.
  {ok, API}.

get_settings_for_domain(Domain) ->
  todo.
get_settings_for_api(ApiID) ->
  todo.
get_settings_for_developer(DeveloperId) ->
  todo.
get_settings_for_app_user(UserId) ->
  todo.
