-module(auth_key).
-export([auth/2, parse_basic/2, parse_auth_header/1, auth_parameters/1]).

parse_auth_header(<<"Bearer ", R/binary>>) when R =/= <<>> ->
    % TODO: validate R.
    {bearer, R};
parse_auth_header(<<"Basic ", R/binary>>) ->
    parse_basic(base64:decode(R), <<>>).

parse_basic(<< $:, Password/binary >>, UserID) ->
    {basic, UserID, Password};
parse_basic(<< C, R/binary >>, UserID) ->
    parse_basic(R, << UserID/binary, C >>).

auth_parameters(_Config) ->
    [{header, <<"authorization">>, strip}].

auth(AuthInfo, API) ->
    % TODO: Have reverse lookup table
    case AuthInfo of
        [{header, <<"authorization">>, undefined}] -> deny;
        [{header, <<"authorization">>, Header}] ->
            {bearer, Key} = parse_auth_header(Header),
            Developer = identify_key(Key),
            case proxy42_authorisation:is_authorised(Developer, API) of
                true -> allow;
                _ -> deny
            end
    end.

identify_key(Key) ->
    case mnesia:dirty_match_object({developer, '_', Key, '_'}) of
        [] -> deny;
        [{_, Id, _, _}] -> Id
    end.

