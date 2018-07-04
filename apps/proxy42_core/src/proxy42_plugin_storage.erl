-module(proxy42_plugin_storage).
-compile(export_all).

insert(Tab, Entry) ->
    mnesia:transaction(
      fun()->
          mnesia:write(Tab, Entry, write)
      end).

update(Tab, Key, UpdateFn) when is_function(UpdateFn) ->
  mnesia:transaction(
    fun()->
      Entry = ?MODULE:get(Tab,Key),
      NewEntry = apply(UpdateFn, Entry),
      mnesia:write(Tab, NewEntry, write)
    end).

get(Tab, Key) ->
    mnesia:dirty_read(Tab, Key).

query(Tab, Pattern) ->
    mnesia:dirty_match_object(Tab, Pattern).

delete(Tab, Key) ->
    mnesia:delete(Tab, Key, write).

increment_counter(Tab, Key, Inc) ->
  mnesia:dirty_update_counter(Tab, Key, Inc).
