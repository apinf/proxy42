-module(p42_rl_ftbucket).
-export([init/0, terminate/1]).
-export([check/2, peek/2, setup/3, reset_counters/1]).

-export([reset_counters/2]).
-import(p42_req_ctx, [get_rate_limit/1, get_api_id/1]).

-define(RLSTAB, rate_limit_state).
-define(RLCTAB, rate_limit_counter).

-record(?RLSTAB, {bucket, limit, period, reset_time}).
-record(?RLCTAB, {bucket, counter}).

init() ->
  RateLimitStateTab = {?RLSTAB, record_info(fields, rate_limit_state)},
  RateLimitCounterTab = {?RLCTAB, record_info(fields, rate_limit_counter)},
  Opts = [
          {tables, [RateLimitStateTab, RateLimitCounterTab]},
          {strategies, [{rate_limit , ?MODULE}]}
          ],
  timer:start(),
  {ok, Opts}.

terminate(_Reason) ->
  ok.

setup(Bucket, RateLimit, RatePeriod) ->
  RLState = #rate_limit_state{bucket=Bucket,
                              limit=RateLimit,
                              period=RatePeriod,
                              reset_time=erlang:system_time(millisecond)},
  {atomic, ok} = proxy42_plugin_storage:insert(?RLSTAB, RLState),
  {ok, _} = timer:apply_after(RatePeriod, ?MODULE, reset_counters, [Bucket, RatePeriod]),
  RLState.

check(RLTag, ReqCtx) ->
  Bucket = get_bucket(RLTag, ReqCtx),
  #rate_limit_state{limit=Limit,
                    period=Period,
                    reset_time=Reset} =
    case proxy42_plugin_storage:get(?RLSTAB, Bucket) of
      [] ->
        SetupLimit = get_rate_limit(ReqCtx),
        Period_=5000, % TODO: Hardcoded Period
        setup(Bucket, SetupLimit, Period_);
      [RLState] -> RLState
    end,
  Count = proxy42_plugin_storage:increment_counter(?RLCTAB, Bucket, 1),
  rate_limit_result(Count, Limit, Reset, Period).

peek(RLTag, ReqCtx) ->
  Bucket = get_bucket(RLTag, ReqCtx),
  case proxy42_plugin_storage:get(?RLSTAB, Bucket) of
    [] ->
      % TODO: Think this
      rate_not_set;
    [#rate_limit_state{limit=Limit, period=Period, reset_time=PreviousReset}] ->
      Reset = next_reset_ts(PreviousReset, Period),
      case proxy42_plugin_storage:get(?RLCTAB, Bucket) of
        [Count] ->
          {Limit, Limit - Count, Reset};
        [] ->
          {0, Limit, Reset}
      end
    end.

rate_limit_result(Count, Limit, Reset, Period) when Count =< Limit ->
  {allow, Limit, Limit - Count, next_reset_ts(Reset, Period)};
rate_limit_result(_Count, _Limit, Reset, Period) ->
  RetryAfter = next_reset_in(Reset, Period),
  {deny, RetryAfter}.

reset_counters(Bucket) ->
  {atomic, ok} = proxy42_plugin_storage:update(?RLSTAB, Bucket,
                                               fun(E) -> E#?RLSTAB{reset_time = erlang:system_time(millisecond)} end),
  ok = mnesia:dirty_write(#rate_limit_counter{bucket=Bucket,
 counter=0}).

reset_counters(Bucket, RatePeriod) ->
  reset_counters(Bucket),
  {ok, _} = timer:apply_after(RatePeriod, ?MODULE, reset_counters, [Bucket, RatePeriod]).

next_reset_in(PreviousReset, Period) ->
  (Period - (erlang:system_time(millisecond) - PreviousReset)) div 1000.

next_reset_ts(PreviousReset, Period) ->
  (PreviousReset + Period) div 1000.

get_bucket(RLTag, ReqCtx) when is_binary(RLTag) ->
  APIId = get_api_id(ReqCtx),
  <<APIId/binary, ":", RLTag/binary>>.
