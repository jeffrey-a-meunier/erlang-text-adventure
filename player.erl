-module(player).
-export([addItem/2, getItems/1, getLocation/1, removeItem/2, setLocation/2]).

addItem(Db, Item) ->
    db:add(Db, player, items, Item).

getItems(Db) ->
    db:get(Db, player, items).

getLocation(Db) ->
    [Location] = db:get(Db, player, location),
    Location.

removeItem(Db, Item) ->
    db:del(Db, player, items, Item).

setLocation(Db, Location) ->
    db:set(Db, player, location, Location).
