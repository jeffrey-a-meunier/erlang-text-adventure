-module(location).
-export([addItem/3, getExits/2, getItems/2, getDestination/3,
         removeItem/3, show/1]).

addItem(Db, Location, Item) ->
    db:add(Db, location, {Location, items}, Item).

getExits(Db, Location) ->
    db:get(Db, location, {Location, exits}).

getItems(Db, Location) ->
    db:get(Db, location, {Location, items}).

getDestination(Db, Location, Exit) ->
    db:get(Db, location, {Location, destination, Exit}).

removeItem(Db, Location, Item) ->
    db:del(Db, location, {Location, items}, Item).

show(Db) ->
    Location = player:getLocation(Db),
    io:format("Location: ~s~n", [Location]),
    Items = getItems(Db, Location),
    ItemString = utils:stringify(Items),
    io:format("Items: ~s~n", [ItemString]),
    Exits = getExits(Db, Location),
    ExitString = utils:stringify(Exits),
    io:format("Exits: ~s~n", [ExitString]),
    ok.
