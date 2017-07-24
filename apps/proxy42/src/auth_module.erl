-module(auth_module).

-type request():: cowboyku_req:req().
-type credential() :: string().
-type mode() :: keep | strip.
-type handlerstate() :: term().
-type user() :: term().
-type authresult() :: allow | deny | {rate_limit, user()}.
% -spec auth(credential(), mode(), request(), handlerstate()) -> {authresult(), request(), handlerstate()}.

% auth(Credential) ->
%     {allow, Req, HandlerState}.
