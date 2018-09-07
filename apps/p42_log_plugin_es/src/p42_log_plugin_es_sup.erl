-module(p42_log_plugin_es_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => rest_for_one, intensity => 2, period => 10},
  ChildSpecs = [#{id => p42_es_logger_sup,
                  start => { p42_es_logger_sup, start_link, []},
                  type => supervisor}
                ],
  {ok, {SupFlags, ChildSpecs}}.
