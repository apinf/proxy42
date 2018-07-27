-module(p42_auth_always).
-export([init/0, terminate/1, auth/3]).

init() ->
  Opts = [{strategies, [{auth, ?MODULE}]}],
  {ok, Opts}.

terminate(_Reason) ->
   ok.

auth(_ConfigId, _Req, _ReqCtx) ->
    ignore_rate_limit.
