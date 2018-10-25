-module(p42_auth_key).
-export([init/0, terminate/1]).
-export([auth/3]).
-export([handle_http/2]).

-import(proxy42_authorisation, [is_authorised/2]).
-import(p42_req_ctx, [get_api_id/1]).
-import(p42_req, [get_header/2, get_qs_val/2]).

-define(KEYSTAB, auth_key_developers).

-define(PLUG, 'Elixir.Plug.Conn').

init() ->
  %% {sup, pid} is the supervision tree of plugin if it exists.
  %% {strategies, [{strategy_type, strategy_name, strategy_module}]}
  %% slug, string | binary
  %% tables, [{tablename, attributes}]
  KeysTab = {?KEYSTAB, [key, developer_id]},
  Opts = [
          {strategies, [{auth, <<"auth_key">>, ?MODULE}]},
          {slug, <<"auth_key">>},
          {tables, [KeysTab]}
          ],
  {ok, Opts}.

terminate(_Reason) ->
  ok.

%% TODO: add table to hold this stuff
%% Allow configurable header name
get_config(<<"bearer-auth">>) ->
  %% header/qs_val, Name, raw/bearer , keep/strip
  {header, <<"authorization">>, bearer, strip}.

auth(ConfigId, Req, ReqCtx) ->
  Config = get_config(ConfigId),
  %% TODO: Have reverse lookup table
  handle_config(Config, Req, ReqCtx).

handle_config({header, Hdr, Kind, _Strip}, Req, ReqCtx) ->
  HdrVal = get_header(Hdr, Req),
  %% Missing header value is okay.
  %% will EXIT, caught and turned to deny by router
  Key = case Kind of
          raw -> HdrVal;
          bearer -> bearer_key(HdrVal)
        end,
  DeveloperId = identify_key(Key),
  APIId = get_api_id(ReqCtx),
  case is_authorised(DeveloperId, APIId) of
    true -> DeveloperId; % Or {developer id, userid}
    _ -> deny
  end.

%% -type status() :: non_neg_integer().
%% -type body() :: iolist().
%% -type headers() :: [{binary(), binary()}].
%% -type conn() :: any. % FIXME: plug.conn
%% -spec handle_http(binary(), conn()) ->
%%                         {ok, status(), headers(), body()}
%%                         | {error, term()}.
%% TODO: Create a better route
%% TODO: check if dev id exists
handle_http([<<"issue_key">>], Conn) ->
  %% Status = 256,
  %% ResponseHeaders = [],
  %% ResponseBody = [],
  %% {ok, Status, ResponseHeaders, ResponseBody},
  DeveloperId = maps:get(<<"developer_id">>, maps:get(body_params, Conn)),
  Key = issue_key(DeveloperId),
  ?PLUG:send_resp(Conn, 201, ["{\"key\":\"", Key, "\"}"]).

identify_key(Key) ->
  case proxy42_plugin_storage:get(?KEYSTAB, Key) of
    [] -> deny;
    [{?KEYSTAB, Key, Id}] -> Id
  end.

%% XXX: also accept developer record?
issue_key(DeveloperId) ->
  Key = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  Entry = {?KEYSTAB, Key, DeveloperId},
  {atomic, ok} = proxy42_plugin_storage:insert(?KEYSTAB, Entry),
  Key.

bearer_key(<<"Bearer ", R/binary>>) when R =/= <<>> ->
  R.
