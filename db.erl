% Raw database interface.

-module(db).
-export([add/4, allBindings/1, clear/1, close/1, del/3, del/4,
         get/3, keys/1, open/1, set/4]).

add(Db, Table, Key, Value) ->
    dets:insert(Db, {{Table, Key}, Value}).

allBindings(Db) ->
    dets:traverse(Db, fun (X) -> {continue, X} end).

clear(Db) ->
    dets:delete_all_objects(Db).

close(Db) ->
    dets:close(Db).

% Deletes all values.
del(Db, Table, Key) ->
    Key1 = {Table, Key},
    case dets:lookup(Db, Key1) of
        [] ->
            error;
        _ ->
            dets:delete(Db, {Table, Key})
    end.

% Deletes a single value.
del(Db, Table, Key, Value) ->
    Key1 = {Table, Key},
    case dets:lookup(Db, Key1) of
        [] ->
            error;
        _ ->
            dets:delete_object(Db, {{Table, Key}, Value})
    end.

get(Db, Table, Key) ->
    Res = dets:lookup(Db, {Table, Key}),
    [Value || {_, Value} <- Res].

keys(Db) ->
    First = dets:first(Db),
    keys(Db, First, []).

keys(_Db, '$end_of_table', Acc) ->
    Acc;
keys(Db, Key, Acc) ->
    keys(Db, dets:next(Db, Key), [Key | Acc]).

open(DbName) ->
    dets:open_file(DbName, [{type, bag}]).

set(Db, Table, Key, Value) ->
    del(Db, Table, Key),
    dets:insert(Db, {{Table, Key}, Value}).
