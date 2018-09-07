-module(proxy42_authenticate_middleware).
-behaviour(cowboyku_middleware).
-export([execute/2]).

-type env() :: cowboyku_middleware:env().

-spec execute(p42_req:req(), env()) -> {ok, p42_req:req(), env()} |
                                       {error, 403 | 429, p42_req:req()} .
execute(Req, Env) ->
  {Log, Req1} = cowboyku_req:meta(logging, Req),
  Log1 = vegur_req_log:stamp(pre_auth, Log),
  Req2 = cowboyku_req:set_meta(logging, Log1, Req1),
  {InterfaceModule, HandlerState, Req3} = vegur_utils:get_interface_module(Req2),
  {AuthResult, Req4, HandlerState3} = InterfaceModule:auth(Req3, HandlerState),
  Req5 = vegur_utils:set_handler_state(HandlerState3, Req4),
  case AuthResult of
    ignore_rate_limit -> {ok, Req5, Env};
    deny -> {error, 403, Req5};
    RLTag -> handle_rate_limit(RLTag, Req5, Env)
  end.

-spec handle_rate_limit(proxy42_router:rltag(), p42_req:req(), env()) ->
                           {ok, p42_req:req(), env()}
                             | {error, 429, p42_req:req()}.
handle_rate_limit(RLTag, Req, Env) ->
  {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
  {RL, HandlerState1} = InterfaceModule:rate_limit(RLTag, Req1, HandlerState),
  Req2 = vegur_utils:set_handler_state(HandlerState1, Req1),
  case RL of
    allow ->
      {ok, Req2, Env};
    {allow, Limit, Remaining, Reset} ->
      %% BCP 178 deprecates X- prefix in headers.
      %% https://tools.ietf.org/html/bcp178
      L = str(Limit), Rem = str(Remaining), Rst = str(Reset),
      Req3 = cowboyku_req:set_resp_header(<<"RateLimit-Limit">>, L, Req2),
      Req4 = cowboyku_req:set_resp_header(<<"RateLimit-Remaining">>, Rem, Req3),
      Req5 = cowboyku_req:set_resp_header(<<"RateLimit-Reset">>, Rst, Req4),
      {ok, Req5, Env};
    deny ->
      {error, 429, Req2};
    {deny, RetryAfter} ->
      RA = str(RetryAfter),
      Req3 = cowboyku_req:set_resp_header(<<"Retry-After">>, RA, Req2),
      {error, 429, Req3}
  end.

-spec str(term()) -> iodata().
str(X) when is_integer(X) -> erlang:integer_to_binary(X);
str(X) when is_binary(X) -> X;
str(X) when is_list(X) -> X.
