-module(proxy42_storage).
-include("domain_group.hrl").
-include("developers.hrl").
-include("authorization.hrl").

-export([start/0, start/2, init/0, stop/1]).
-export([find_domain_group/1, ensure_table/2]).

%% announce
-record(storage_announce, {sender, node}).
%% ack announce
-record(storage_ack_announce, {sender, node}).

start(_StartType, _StartArgs) ->
    start().

start() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    {ok, Pid}.

init() ->
    %% TODO: Take in a list of distributed hosts.
    Nodes = net_adm:world_list([]),
    NodeCount = length(Nodes),
    case NodeCount > 1 of
        true ->
            init_storage(false),
            announce_storage_to_nodes(Nodes);
       false ->
            init_storage(true)
    end,
    loop([]).

init_storage(false) ->
    mnesia:start();
init_storage(true) ->
    init_storage(false),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    create_tables(),
    wait_for_tables().

create_tables() ->
    ok = ensure_table(domain_group, [{attributes, record_info(fields, domain_group)}
                                       ,{type, set}
                                      ,{disc_copies, [node()]}]),
    ok = ensure_table(developer, [{attributes, record_info(fields, developer)}
                                  ,{type, set}
                                   ,{disc_copies, [node()]}]),
    ok = ensure_table(authorization, [{attributes, record_info(fields, authorization)}
                                   ,{type, bag}
                                   ,{disc_copies, [node()]}]).
ensure_table(Tab, Opts) ->
    case mnesia:create_table(Tab, Opts) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, Tab}} ->
            IntendedAttrs = proplists:get_value(attributes, Opts),
            TableAttrs = mnesia:table_info(Tab, attributes),
            case IntendedAttrs == TableAttrs of
                true -> ok;
                false -> {error, attributes}
            end
    end.

wait_for_tables() ->
    mnesia:wait_for_tables([domain_group, developer, authorization], 10000).

announce_storage_to_nodes([Node|T]) when Node =:= node() ->
    announce_storage_to_nodes(T);
announce_storage_to_nodes([Node|T]) ->
    Pid = rpc:call(Node, erlang, whereis, [storage]),
    Pid ! #storage_announce{sender=self(), node=node()},
    announce_storage_to_nodes(T);
announce_storage_to_nodes([]) -> ok.

loop(State) ->
    receive
        #storage_ack_announce{sender=_Sender, node=_Node} ->
            loop(State);
        #storage_announce{sender=Sender, node=Node} ->
            copy_storage_to_node(Node),
            Sender ! #storage_ack_announce{sender=self(), node=node()},
            loop(State);
        _M ->
            loop(State)
    end.

copy_storage_to_node(Node) ->
    _R1 = mnesia:change_config(extra_db_nodes, [Node]),
    _R2 = mnesia:change_table_copy_type(schema, Node, disc_copies),
    copy_tables(Node).

copy_tables(Node) ->
    _R1 = mnesia:add_table_copy(domain_group, Node, disc_copies),
    _R1 = mnesia:add_table_copy(developer, Node, disc_copies),
    _R1 = mnesia:add_table_copy(authorization, Node, disc_copies).

stop(_State) ->
    ok.

find_domain_group(Pattern) ->
    [DomainGroup] = mnesia:dirty_match_object(Pattern),
    DomainGroup.
