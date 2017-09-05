-module(proxy42).
-export([config/1]).

-define(APP, proxy42_core).
config(Key) ->
    {ok, Value} = application:get_env(?APP, Key),
    Value.
