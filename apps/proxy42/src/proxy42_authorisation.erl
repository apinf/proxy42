-module(proxy42_authorisation).
-export([is_authorised/2]).

is_authorised(Developer, API) ->
    case mnesia:dirty_match_object({authorization, Developer, API}) of
        [] -> false;
        [_] -> true
    end.
