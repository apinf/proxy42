%%%-------------------------------------------------------------------
%% @doc myapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(proxy42_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  Config = #{ strategy => one_for_all },
  Endpoint = #{ id => proxy42_endpoint,
                start => {proxy42_endpoint, start_link, [proxy42:config(port)]}
              },
  Storage = #{ id => proxy42_storage,
               start => {proxy42_storage, start, []}
             },
  %%TODO: Create and add plugin supervisor
  PluginManager = #{ id => proxy42_plugins,
                     start => {proxy42_plugins, start, []}},
  Children = [Storage, Endpoint, PluginManager],
  {ok, {Config, Children}}.
%%====================================================================
%% Internal functions
%%====================================================================
