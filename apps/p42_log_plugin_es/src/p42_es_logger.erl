-module(p42_es_logger).

% Gen server callbacks
-export([start_link/0, start_link/1
        ,init/1, terminate/2
        ,handle_info/2, handle_cast/2
        ,format_status/2
        ]).

% Public API
-export([log/1]).

-define(POOL, p42_es_logger_pool).
%% @todo: use ?MODULE instead of APP
-define(APP, p42_log_plugin_es).

%%
-record(state, {
   url   = undefined :: string()
,  batch = undefined :: integer()
,  q     = undefined :: datum:q()
}).


%%%-------------------------------------------------------------------
%% @doc Logs supplied document to elasticsearch
%%  The server and urn are taken from application's environment
%% @end
%%%-------------------------------------------------------------------
log(Doc) ->
  Worker = poolboy:checkout(?POOL),
  gen_server:cast(Worker, {log, Doc}).

%%%-------------------------------------------------------------------
%% Gen server callbacks
%%%-------------------------------------------------------------------
start_link() ->
  start_link([]).

start_link(_Args) ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
   {ok, _TRef} = timer:send_interval(timer:minutes(1), sync),
   {ok,
      #state{
         url   = config_elastic_url()
      ,  batch = config_elastic_batch()
      ,  q     = q:new()
      }
   }.

terminate(Reason, #state{q = Q}) ->
   io:format("PID ~p terminating for reason ~p. Queue len: ~p~n",
            [self(), Reason, q:length(Q)]),
   ok.

handle_info(sync, #state{url = Url, q = Q} = State) ->
   case q:length(Q) of
      L when L == 0 ->
         {noreply, State};
      _ ->
         send_req(Url, Q), 
         {noreply, State#state{q = q:new()}}
   end;

handle_info(Info, #state{} = State) ->
   io:format("Unhandled info: ~p~n", [Info]),
   {noreply, State}.

handle_cast({log, Doc}, #state{url = Url, batch = N, q = Queue0} = State) ->
   Queue1 = enq(Doc, Queue0),
   Queue2 = case q:length(Queue1) of
      L when L >= N ->
         send_req(Url, Queue1),
         q:new();
      _ ->
         Queue1
   end,
   poolboy:checkin(?POOL, self()),
   {noreply, State#state{q = Queue2}}.

format_status(_Opt, [_Pdict, #state{q = Q}]) ->
  [{data, [{"Queue size", q:length(Q)}] }].

%%%-------------------------------------------------------------------
%% Internal helpers
%%%-------------------------------------------------------------------
enq(Doc, Queue) ->
   Preamble = <<"{\"index\": {}}\n">>,
   Entry = [Preamble, jsx:encode(Doc), <<"\n">>],
   q:enq(Entry, Queue).

send_req(Url, Q) ->
   Headers = [{<<"Content-Type">>, <<"application/x-ndjson">>}],
   Body = q:list(Q),
   hackney:request(post, Url, Headers, Body, [with_body]).

%%
config_elastic_url() ->
   os:getenv(
      "CONFIG_ELASTIC_URL",
      application:get_env(?APP, es_url, "http://localhost:9200/p42logs/_bulk")
   ).

%%
config_elastic_batch() ->
   erlang:list_to_integer(
      os:getenv(
         "CONFIG_ELASTIC_BATCH",
         application:get_env(?APP, es_bulk_size, "1000")
      )
   ).
