-module(auth_key).
-export([init/0, terminate/1]).
-export([auth/2, auth_parameters/1]).
-export([handle_http/2]).

-import(proxy42_authorisation, [is_authorised/2]).

-define(KEYSTAB, auth_key_developers).

-define(PLUG, 'Elixir.Plug.Conn').

parse_auth_header(<<"Bearer ", R/binary>>) when R =/= <<>> ->
    % TODO: validate R.
    {bearer, R};
parse_auth_header(<<"Basic ", R/binary>>) ->
    parse_basic(base64:decode(R), <<>>).

parse_basic(<< $:, Password/binary >>, UserID) ->
    {basic, UserID, Password};
parse_basic(<< C, R/binary >>, UserID) ->
    parse_basic(R, << UserID/binary, C >>).

init() ->
   % {sup, pid} is the supervision tree of plugin if it exists.
   % {strategies, [{strategy_type, strategy_name}]}
   % slug, string | binary
   % tables, [{tablename, attributes}]
   KeysTab = {?KEYSTAB, [key, developer_id]},
   Opts = [
           {slug, <<"auth_key">>},
           {tables, [KeysTab]},
           {strategies, [{auth, auth_key}]}
          ],
   {ok, Opts}.

terminate(_Reason) ->
    ok.

auth_parameters(_Config) ->
    [{header, <<"authorization">>, strip}].

auth(AuthInfo, APIId) ->
  % TODO: Have reverse lookup table
  case AuthInfo of
    [{header, <<"authorization">>, undefined}] -> deny;
    [{header, <<"authorization">>, Header}] ->
      {bearer, Key} = parse_auth_header(Header),
      DeveloperId = identify_key(Key),
      case is_authorised(DeveloperId, APIId) of
        true -> DeveloperId; % Or {developer id, userid}
        _ -> deny
      end
  end.

%% -type status() :: non_neg_integer().
%% -type body() :: iolist().
%% -type headers() :: [{binary(), binary()}].
%% -type conn() :: any. % FIXME: plug.conn
%% -spec handle_http(binary(), conn()) ->
%%                         {ok, status(), headers(), body()}
%%                         | {error, term()}.
% TODO: Create a better route
% TODO: check if dev id exists
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

% XXX: also accept developer record?
issue_key(DeveloperId) ->
    Key = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Entry = {?KEYSTAB, Key, DeveloperId},
    {atomic, ok} = proxy42_plugin_storage:insert(?KEYSTAB, Entry),
    Key.
