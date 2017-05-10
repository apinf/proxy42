-module(proxy42_config).
-export([domain_config/2]).

%% @doc Find the 
domain_config(_Domain, Path = <<"/httpbin", _/binary>>) ->
  DomainGroup = #{
    hostname => "httpbin.org"
   ,frontend_prefix => "/httpbin/"
   ,backend_prefix => "/"
   ,servers => [
                {http, "httpbin.org", 80}
               % ,{http, "eu.httpbin.org", 80}
               % ,{https, "httpbin.org", 443}
               ]
   ,strategy => random
    % request and response
   ,additional_headers => {[],[]}
   ,ratelimit => whatever
   },
  % DomainGroup can be an arbitrary datastructure.
  % This gets passed to checkout_service,
  %   which returns a service, again an arbitrary data structure
  % The service gets passed to service_backend which should return
  % domain name/ip and port.
  {ok, DomainGroup};

domain_config(_Domain, Path = <<"/postman-echo", _/binary>>) ->
  DomainGroup = #{
    hostname => "postman-echo.com"
   ,frontend_prefix => "/postman-echo/"
   ,backend_prefix => "/"
   ,servers => [
                {http, "postman-echo.com", 80}
               ]
   ,strategy => random
    % request and response
   ,additional_headers => {[],[]}
   ,ratelimit => whatever
   },
  % DomainGroup can be an arbitrary datastructure.
  % This gets passed to checkout_service,
  %   which returns a service, again an arbitrary data structure
  % The service gets passed to service_backend which should return
  % domain name/ip and port.
  {ok, DomainGroup}.
