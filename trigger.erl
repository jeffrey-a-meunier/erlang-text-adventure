-module(trigger).
-export([activate_pre/4, activate_post/4]).

% returns true if successful, else false
activate_pre(Db, Verb, Location, Item) ->
    activate(Db, trigger_pre, Verb, Location, Item).

activate_post(Db, Verb, Location, Item) ->
    activate(Db, trigger_post, Verb, Location, Item).

activate(Db, Table, Verb, Location, Item) ->
    Triggers = get(Db, Table, Verb, Location, Item),
    exec(Db, Triggers).

exec(Db, [{add, Table, Key, Value} | Triggers]) ->
    [db:add(Db, Table, Key, Value1) || Value1 <- Value],
    exec(Db, Triggers);
exec(Db, [{del, Table, Key} | Triggers]) ->
    db:del(Db, Table, Key),
    exec(Db, Triggers);
exec(Db, [{del, Table, Key, Value} | Triggers]) ->
    db:del(Db, Table, Key, Value),
    exec(Db, Triggers);
exec(Db, [{echo, Message} | Triggers]) ->
    io:format("~s~n", [Message]),
    exec(Db, Triggers);
exec(_Db, [{return, Res} | _Triggers]) ->
    Res;
exec(Db, [{set, Table, Key, Value} | Triggers]) ->
    db:set(Db, Table, Key, Value),
    exec(Db, Triggers);
exec(_Db, [end_game | _Triggers]) ->
    end_game;
exec(Db, [Trigger | Triggers]) ->
    io:format("trigger:exec unhandled trigger ~p~n", [Trigger]),
    exec(Db, Triggers);
exec(_Db, []) ->
    true.

get(Db, Table, Verb, Location, Item) ->
    db:get(Db, Table, {Verb, Location, Item}).
