-module(p42_log_plugin_es).
-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1, config_change/3]).

%% Plugin API
-export([init/0, log/2]).

start(_Mode, _Opts) ->
  ok = proxy42_plugins:register_plugin(?MODULE),
  p42_log_plugin_es_sup:start_link().

prep_stop(State) ->
  proxy42_plugins:unregister_plugin(?MODULE),
  State.

stop(_State) ->
  ok.

config_change(_Changed, _Added, _Removed) ->
  %% If the app config is used somewhere in any way that
  %% won't automatically update, or if the config is used
  %% to make decisions at start time, re-evaluate those
  %% in face of updated config.
  ignore.


init() ->
  Opts = [{strategies, [{log, ?MODULE}]}],
  {ok, Opts}.

log(LogInfo, ReqCtx) ->
  Events = lists:map(fun format_event/1, LogInfo),
  %% TODO: Add developer_id and user_id to log.
  Meta = lists:map(fun(X) -> {X, p42_req_ctx:ctx_get(X, ReqCtx)} end,
                   [incoming_ip, domain, method, path,
                    request_id, api_id, developer_id, user_id,
                    response_code]),
  Doc = [ {events, Events} | Meta],
  p42_es_logger:log(Doc).

format_event({Time, Event, no_details}) ->
  format_event(Time, Event);
format_event({Time, Event, Detail}) when is_atom(Detail) ->
  format_event(Time, key(Event,Detail)).
format_event(Time, Event) ->
  {Event, timestamp(Time)}.

key(E, D) ->
  E1 = erlang:atom_to_binary(E, utf8),
  D1 = erlang:atom_to_binary(D, utf8),
  <<E1/binary, ":" ,D1/binary>>.

%%%-------------------------------------------------------------------
%% @private
%% @doc Converts given erlang:timestamp() into milliseconds since
%%  epoch.
%% @end
%%%-------------------------------------------------------------------
timestamp({MegaSecs, Secs, MicroSecs}) ->
  (MegaSecs * 1000000 + Secs)*1000 + (MicroSecs div 1000).
