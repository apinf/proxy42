-module(proxy42).
-export([config/1]).

config(Key) ->
    {ok, Value} = application:get_env(proxy42, Key),
    Value.
