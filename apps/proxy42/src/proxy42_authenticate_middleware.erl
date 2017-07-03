-module(proxy42_authenticate_middleware).
-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
  {Log, Req1} = cowboyku_req:meta(logging, Req),
  Log1 = vegur_req_log:stamp(pre_auth, Log),
  Req2 = cowboyku_req:set_meta(logging, Log1, Req1),
  {InterfaceModule, HandlerState, Req3} = vegur_utils:get_interface_module(Req2),
  % Authconfig is a list of tuples {Type, Key, Mode}
  % Type can be header or qs (Query string)
  % Key should be name of header or query parameter depending on Type
  % Mode can be keep or strip, and will decide if the header or query param
  % will be consumed by us or retained in the request.
  {AuthConfig, Req4, HandlerState1} = InterfaceModule:auth_config(Req3, HandlerState),
  AuthInfo = lists:filtermap(fun (X)-> extract(X, Req4) end, AuthConfig),
  {AuthResult, Req5, HandlerState2} = InterfaceModule:auth(AuthInfo, Req4, HandlerState1),
  Req6 = vegur_utils:set_handler_state(HandlerState2, Req5),
  case AuthResult of
    allow -> {ok, Req6, Env};
    {rate_limit, User} -> handle_rate_limit(User, Req6, Env);
    deny -> {halt, 403, Req6}
  end.


handle_rate_limit(User, Req, Env) ->
  {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
  {RL, HandlerState1} = InterfaceModule:rate_limit(User, Req1, HandlerState),
  Req2 = vegur_utils:set_handler_state(HandlerState1, Req1),
  case RL of
    allow ->
      {ok, Req2, Env};
    {allow, Limit, Remaining, Reset} ->
      % BCP 178 deprecates X- prefix in headers.
      % https://tools.ietf.org/html/bcp178
      L = str(Limit), Rem = str(Remaining), Rst = str(Reset),
      Req3 = cowboyku_req:set_resp_header(<<"RateLimit-Limit">>, L, Req2),
      Req4 = cowboyku_req:set_resp_header(<<"RateLimit-Remaining">>, Rem, Req3),
      Req5 = cowboyku_req:set_resp_header(<<"RateLimit-Reset">>, Rst, Req4),
      {ok, Req5, Env};
    deny ->
      {halt, 429, Req2};
    {deny, RetryAfter} ->
      RA = str(RetryAfter),
      Req3 = cowboyku_req:set_resp_header(<<"Retry-After">>, RA, Req2),
      {halt, 429, Req3}
  end.

extract({header, Header, Mode}, Req) ->
  case cowboyku_req:header(Header, Req) of
    {undefined, Req} -> false;
    {Val, Req} -> {true, parse_auth_header(Val)}
  end;
extract({qs, Param, Mode}, Req) ->
  %% TODO
  case cowboyku_req:qs_val(Param, Req) of
    undefined -> false;
    Val -> {true, Val}
  end.

parse_auth_header(<<"Bearer ", R/bits>>) when R =/= <<>> ->
    % TODO: validate R.
    erlang:display(R),
    {bearer, R};
parse_auth_header(<<"Basic ", R/bits>>) ->
    parse_basic(base64:decode(R), <<>>).

parse_basic(<< $:, Password/bits >>, UserID) ->
 erlang:display(<<UserID/binary, " ", Password/binary>>),
 {basic, UserID, Password};
parse_basic(<< C, R/bits >>, UserID) ->
 parse_basic(R, << UserID/binary, C >>).


str(X) when is_integer(X) -> erlang:integer_to_binary(X);
str(X) when is_binary(X) -> X;
str(X) when is_list(X) -> X.
