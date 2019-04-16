%%
%% @doc
%%   core api for logger plug-ins
-module(proxy42_api_logger).

-export([message/2]).

%% TODO: Add developer_id and user_id to log.
-define(METADATA, [
   incoming_ip
,  domain
,  method
,  path
,  request_id
,  api_id
,  developer_id
,  user_id
,  response_code
]).

%%
%% @doc
%%   transforms internal context into logger report
-spec message(_, _) -> _.

message(LogInfo, Request) ->
   [message_events(LogInfo) | message_metadata(Request)].

message_metadata(Request) ->
   lists:map(
      fun(X) -> {X, p42_req_ctx:ctx_get(X, Request)} end,
      ?METADATA
   ).

message_events(LogInfo) ->
   {events, lists:map(fun format_event/1, LogInfo)}.


format_event({Time, Event, no_details}) ->
  format_event(Time, Event);
format_event({Time, Event, Detail}) when is_atom(Detail) ->
  format_event(Time, key(Event, Detail)).

format_event(Time, Event) ->
  {Event, timestamp(Time)}.

key(E, D) ->
  E1 = erlang:atom_to_binary(E, utf8),
  D1 = erlang:atom_to_binary(D, utf8),
  <<E1/binary, ":" ,D1/binary>>.

timestamp({MegaSecs, Secs, MicroSecs}) ->
  (MegaSecs * 1000000 + Secs) * 1000 + (MicroSecs div 1000).
