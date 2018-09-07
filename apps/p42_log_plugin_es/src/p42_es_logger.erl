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
-define(APP, p42_log_plugin_es).

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
  {ok, Url} = application:get_env(?APP, es_url),
  N = application:get_env(?APP, es_bulk_size, 1000),
  {ok, _TRef} = timer:send_interval(timer:minutes(1), sync),
  {ok, {[], Url, N}}.

terminate(Reason, {Q, _Url, _N}) ->
  io:format("PID ~p terminating for reason ~p. Queue len: ~p~n",
            [self(), Reason, length(Q)]),
  ok.

handle_info(sync, {Q, Url, N} = State) ->
  case length(Q) of
    L when L == 0 -> {noreply, State};
    _ -> send_req(Url, Q), {noreply, {[], Url, N}}
  end;

handle_info(Info, State) ->
  io:format("Unhandled info: ~p~n", [Info]),
  {noreply, State}.

handle_cast({log, Doc}, {Q, Url, N}) ->
  Preamble = <<"{\"index\": {}}\n">>,
  Entry = [Preamble, jsx:encode(Doc), <<"\n">>],
  Q1 = [Entry | Q],
  Q2 = case length(Q1) of
         L when L >= N -> send_req(Url, Q1), [];
         _ -> Q1
       end,
  poolboy:checkin(?POOL, self()),
  {noreply, {Q2, Url, N}}.

format_status(_Opt, [_Pdict, {Q, _Url, _N}]) ->
  [{data, [{"Queue size", length(Q)}] }].

%%%-------------------------------------------------------------------
%% Internal helpers
%%%-------------------------------------------------------------------
send_req(Url, Q) ->
  Headers = [{<<"Content-Type">>, <<"application/x-ndjson">>}],
  Body = lists:reverse(Q),
  hackney:request(post, Url, Headers, Body, [with_body]).
