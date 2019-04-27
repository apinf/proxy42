-module(p42_log_plugin_es).
-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1, config_change/3]).

%% Plugin API
-export([init/0, log/1]).

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

log(Doc) ->
  p42_es_logger:log(Doc).
