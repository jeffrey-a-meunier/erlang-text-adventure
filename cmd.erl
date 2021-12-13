-module(cmd).
-export([handle/2]).

handle(Db, Parts) ->
    Location = player:getLocation(Db),
    Parts1 = Parts#{location => Location},
    case checkPreTrigger(Db, Parts1) of
        true ->
            Res = handleParts(Db, Parts1),
            case Res of
                ok ->
                    case checkPostTrigger(Db, Parts1) of
                        true ->
                            Res;
                        Res1 ->
                            Res1
                    end;
                error ->
                    % the error message has already been displayed
                    ok;
                _ ->
                    Res
            end;
        _ ->
            ok
    end.

checkPreTrigger(Db, #{location:=Location, verb:=Verb, object:=Object}) ->
    trigger:activate_pre(Db, Verb, Location, Object).

checkPostTrigger(Db, #{location:=Location, verb:=Verb, object:=Object}) ->
    trigger:activate_post(Db, Verb, Location, Object).

handleParts(Db, Parts=#{verb:="get"}) ->
    handleGet(Db, Parts);
handleParts(Db, Parts=#{verb:="go"}) ->
    handleGo(Db, Parts),
    ok;
handleParts(Db, #{verb:="look"}) ->
    location:show(Db),
    ok;
handleParts(_Db, #{verb:="help"}) ->
    handleHelp(),
    ok;
handleParts(Db, #{verb:="inv"}) ->
    Items = player:getItems(Db),
    ItemString = utils:stringify(Items),
    io:format("~s~n", [ItemString]),
    ok;
handleParts(_Db, #{verb:="use"}) ->
    % the "use" command is implemented solely by triggers in the database
    ok;
handleParts(Db, Parts=#{verb:="drop"}) ->
    handleDrop(Db, Parts),
    ok;
handleParts(_Db, #{verb:="quit"}) ->
    quit;
handleParts(_Db, #{verb:="exit"}) ->
    quit;
handleParts(_Db, #{verb:="bye"}) ->
    quit;
handleParts(Db, #{verb:="setup"}) ->
    setup:setup(Db),
    ok;
handleParts(_Db, #{verb:=Verb, object:=Object}) ->
    case Object of
        none ->
            io:format("Command not understood: ~s~n", [Verb]);
        _ ->
            io:format("Command not understood: ~s ~s~n", [Verb, Object])
    end,
    error.

%---------------------------------------------------------------------
% The actual command handlers follow

handleGet(Db, #{location:=Location, object:=Item}) ->
    Res = location:removeItem(Db, Location, Item),
    case Res of
        ok ->
            player:addItem(Db, Item);
        _ ->
            io:format("There is no ~s here~n", [Item])
    end.

handleDrop(Db, #{location:=Location, object:=Item}) ->
    case player:removeItem(Db, Item) of
        ok ->
            location:addItem(Db, Location, Item);
        _ ->
            io:format("You are not carrying a ~s.~n", [Item])
    end.

handleGo(Db, #{location:=Location, object:=Exit}) ->
    Location = player:getLocation(Db),
    Exits = location:getExits(Db, Location),
    HasExit = lists:member(Exit, Exits),
    case HasExit of
        true ->
            [Destination] = location:getDestination(Db, Location, Exit),
            player:setLocation(Db, Destination),
            location:show(Db);
        false ->
            io:format("Location ~p does not have exit ~p~n", [Location, Exit])
    end,
    ok.

handleHelp() ->
    io:format("Commands:~n"),
    io:format("  go <exit>    Uses the indicated exit.~n"),
    io:format("  get <item>   Adds the item to the user's inventory.~n"),
    io:format("  drop <item>  Drops the item.~n"),
    io:format("  use <item>   Uses the item for its intended purpose.~n"),
    io:format("  look         Displays information about the user's current location.~n"),
    io:format("  inv          Lists what the user is carrying.~n"),
    io:format("  setup        Resets the database and restarts the game.~n"),
    io:format("  quit | exit | bye~n").
