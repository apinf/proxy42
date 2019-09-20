-module(p42_auth_vienna).

-export([init/0, terminate/1]).
-export([auth/3]).

-import(p42_req, [get_header/2, get_method/1, get_path/1]).

%% Let's not bother with any rate limiting for now.
-define(ALLOW, ignore_rate_limit).

init() ->
  {ok, [
        {strategies,
         [{auth, <<"auth_vienna">>, ?MODULE}]
        }]}.

terminate(_Reason) -> ok.

auth(_ConfigId, Req, _ReqCtx) ->
  XPVPRoles = get_header(<<"x-pvp-roles">>, Req),
  Terms = split(XPVPRoles, <<";">>),
  Terms1 = lists:dropwhile(fun is_not_fiware/1, Terms),
  case Terms1 of
    [] -> deny;
    [T | _] -> check_fiware_service(T, Req)
  end.

is_not_fiware(<<"fiware(", _/binary>>) -> false;
is_not_fiware(_) -> true.

check_fiware_service(<<"fiware(", Perms0/binary>>, Req) ->
  Perms1 = binary_part(Perms0, 0, byte_size(Perms0)-1),
  Perms = lists:foldl(fun collect_perm/2, #{},
                      split(Perms1, <<",">>)),
  Tenant = get_header(<<"fiware-service">>, Req),
  Component = get_fw_component(Req),
  P = maps:get(Tenant, Perms, #{}),
  case maps:get(Component, P, not_found) of
    <<"r">> ->
      case is_read_only(Req) of
        true -> ?ALLOW;
        _  -> deny
      end;
    <<"w">> -> ?ALLOW;
    _ -> deny
  end.

collect_perm(P, Acc) ->
  [Tenant, Rights] = split(P, <<"=">>),
  Perms = lists:foldl(fun(R, A) ->
                          [C,RW] = split(R, <<":">>),
                          maps:put(C, RW, A)
                      end,
                      #{},
                      split(Rights, <<"+">>)),
  maps:put(Tenant, Perms, Acc).

get_fw_component(Req) ->
  Path = get_path(Req),
  [Prefix| _] = split(Path, <<"/">>),
  case Prefix of
    <<"quantumleap">> -> <<"ql">>;
    <<"contextbroker">> -> <<"cb">>;
    P -> P
  end.
  
is_read_only(Req) ->
  case get_method(Req) of
    <<"HEAD">> -> true;
    <<"OPTIONS">> -> true;
    <<"GET">> -> true;
    _ -> false
  end.

split(Str, Sep) ->
  binary:split(Str, Sep, [global, trim_all]).
