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
  Children = [Storage, Endpoint],
  {ok, {Config, Children}}.
%%====================================================================
%% Internal functions
%%====================================================================
