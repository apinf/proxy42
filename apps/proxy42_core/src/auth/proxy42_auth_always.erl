-module(proxy42_auth_always).
-export([auth/2]).

auth(AuthInfo, API) ->
    allow.
