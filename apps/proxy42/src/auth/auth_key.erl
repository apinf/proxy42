-module(auth_key).
-export([auth/2, parse_basic/2, parse_auth_header/1]).

parse_auth_header(<<"Bearer ", R/binary>>) when R =/= <<>> ->
    % TODO: validate R.
    erlang:display(R),
    {bearer, R};
parse_auth_header(<<"Basic ", R/binary>>) ->
    parse_basic(base64:decode(R), <<>>).

parse_basic(<< $:, Password/binary >>, UserID) ->
    erlang:display(<<UserID/binary, " ", Password/binary>>),
    {basic, UserID, Password};
parse_basic(<< C, R/binary >>, UserID) ->
    parse_basic(R, << UserID/binary, C >>).


auth(AuthInfo, Developers) ->
    % TODO: Have reverse lookup table
    case AuthInfo of
        {true, Header} ->
            {bearer, Key} = parse_auth_header(Header),
            check_key(Key, Developers);
        _ -> deny
    end.

check_key(Key, Developers) ->
    case mnesia:dirty_match_object({developer, '_', Key, '_'}) of
        [] -> deny;
        [{_, Id, _, _} | _] -> case lists:member(Id, Developers) of
                                   false -> deny;
                                   true -> allow
                               end
    end.
