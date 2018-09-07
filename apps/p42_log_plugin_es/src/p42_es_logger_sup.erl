-module(p42_es_logger_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},
  PoolName = p42_es_logger_pool,
  PoolArgs = [{name, {local, PoolName}},
              {worker_module, p42_es_logger},
              {strategy, lifo},
              {size, 30}, {max_overflow, 300}],
  WorkerArgs = [],
  ChildSpecs = [poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)],
  {ok, {SupFlags, ChildSpecs}}.
