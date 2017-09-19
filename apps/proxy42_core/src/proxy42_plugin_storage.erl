-module(proxy42_plugin_storage).
-compile(export_all).

insert(Tab, Entry) ->
    mnesia:transaction(
      fun()->
          mnesia:write(Tab, Entry, write)
      end).

get(Tab, Key) ->
    mnesia:dirty_read(Tab, Key).

query(Tab, Pattern) ->
    mnesia:dirty_match_object(Tab, Pattern).

delete(Tab, Key) ->
    mnesia:delete(Tab, Key, write).
