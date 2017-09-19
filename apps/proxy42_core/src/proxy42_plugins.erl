-module(proxy42_plugins).
-export([]).
-compile(export_all).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(APP, proxy42_core).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{hibernate_after, 5000}]).

init(_opts) ->
    {ok, Plugins} = application:get_env(proxy42_core, plugins),
    State = lists:map(fun(Plugin) -> register_plugin_i(Plugin) end, Plugins),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call({register_plugin, Plugin}, _From, State) ->
    NewState = [register_plugin_i(Plugin) | State],
    {reply, ok, NewState, hibernate};

handle_call(get_registered_plugins, _From, State) ->
    {reply, State, State, hibernate}.

handle_cast(_Whatever, State) ->
    {noreply, State, hibernate}.

create_tables([]) ->
    [];
create_tables([Tab| Rest]) ->
    create_table(Tab),
    create_tables(Rest).

% TODO: Detect failures and crash
create_table({Name, Attrs}) ->
    % TODO: Distributiion. Handle table locations and othe props.
    ok = proxy42_storage:ensure_table(Name, [{attributes, Attrs}, {disc_copies, [node()]}]).

register_plugin(Plugin) ->
    gen_server:call(?SERVER, {register_plugin, Plugin}).

register_plugin_i(Plugin) ->
    {ok, Opts} = Plugin:init(),
    %% TODO: Check slug for duplicates
    %% TODO: Then create tables
    %% TODO: Then add to ets/mnesia
    handle_opts(Opts),
    {Plugin, Opts}.

-type plugin() :: atom().
-type opts() :: [{atom(), term()}].
-spec get_registered_plugins() -> [{plugin(), opts()}].
get_registered_plugins() ->
    gen_server:call(?SERVER, get_registered_plugins).

handle_opts([]) ->
    [];
handle_opts([{tables, Tabs}| Rest]) ->
    create_tables(Tabs),
    [{tables, Tabs} | handle_opts(Rest)];
handle_opts([_ | X]) ->
    handle_opts(X).

% TODO: Register slug, register strategies, handle sup tree, get rid of catch all.
% Unknown opts must cause a crash.
