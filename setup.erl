-module(setup).
-export([setup/0, setup/1]).

rules() ->
    %----------------------------------------------------------------
    % location table
    [{location, {"house", items}, ["key", "table", "chair"]},
     {location, {"house", exits}, ["outside"]},
     {location, {"house", destination, "outside"}, ["lawn"]},

     {location, {"lawn", exits}, ["inside", "shed", "stream", "forest"]},
     {location, {"lawn", destination, "inside"}, ["house"]},
     {location, {"lawn", destination, "shed"}, ["shed"]},
     {location, {"lawn", destination, "stream"}, []},
     {location, {"lawn", destination, "forest"}, ["forest"]},

     {location, {"shed", items}, ["axe", "shovel"]},
     {location, {"shed", exits}, ["outside"]},
     {location, {"shed", destination, "outside"}, ["lawn"]},

     {location, {"forest", items}, ["tree"]},
     {location, {"forest", exits}, ["lawn"]},
     {location, {"forest", destination, "lawn"}, ["lawn"]},

     {location, {"meadow", exits}, ["stream"]},

     {location, {"tunnel", items}, ["treasure"]},

     %----------------------------------------------------------------
     % player table
     {player, location, ["house"]},

     %----------------------------------------------------------------
     % trigger table
     {trigger_post, {"get", "house", "key"},
      [{echo, "The key is labeled 'shed'."}]},

     {trigger_pre, {"get", "house", "table"},
      [{echo, "Sorry, it's too big."},
       {return, false}]},

     {trigger_pre, {"go", "lawn", "shed"},
      [{echo, "It's locked. You need to use a key first."},
       {return, false}]},

     {trigger_post, {"use", "lawn", "key"},
      [{echo, "The shed is now unlocked."},
       {del, trigger_pre, {"go", "lawn", "shed"}}]},

     {trigger_pre, {"go", "lawn", "stream"},
      [{echo, "You need to use something to make a bridge."},
       {return, false}]},

     {trigger_post, {"use", "lawn", "log"},
      [{add, location, {"lawn", exits}, ["bridge"]},
       {add, location, {"lawn", destination, "bridge"}, ["meadow"]},
       {add, location, {"meadow", destination, "bridge"}, ["lawn"]},
       {del, player, items, "log"},
       {echo, "You just made a bridge across the stream."}
      ]},

     {trigger_post, {"use", "lawn", "shovel"},
      [{add, location, {"lawn", items}, ["dirt"]}]},

     {trigger_post, {"get", "forest", "log"},
      [{echo, "It looks long enough to bridge a stream."}]},

     {trigger_pre, {"get", "forest", "tree"},
      [{echo, "You need to use something to chop the tree first."},
       {return, false}]},

     {trigger_post, {"use", "forest", "axe"},
      [{add, location, {"forest", items}, ["log"]},
       {echo, "A tree fell in the woods.\nDid it make a sound?"}]},

     {trigger_post, {"use", "forest", "shovel"},
      [{add, location, {"forest", items}, ["dirt"]}]},

     {trigger_post, {"use", "meadow", "shovel"},
      [{add, location, {"meadow", items}, ["dirt"]},
       {add, location, {"meadow", exits}, ["tunnel"]},
       {add, location, {"meadow", destination, "tunnel"}, ["tunnel"]},
       {echo, "You discovered a tunnel!"}]},

      {trigger_post, {"get", "tunnel", "treasure"},
       [{echo, "You won the game!"},
        {return, end_game}]}

    ].

addRules(_Db, []) ->
    ok;
addRules(Db, [Rule | Rules]) ->
    addRule(Db, Rule),
    addRules(Db, Rules).

addRule(Db, {Table, Key, Values}) ->
    Insert = fun (Value) ->
                     db:add(Db, Table, Key, Value)
             end,
    lists:foreach(Insert, Values).

setup() ->
    FileName = main:dbFile(),
    {ok, Db} = db:open(FileName),
    setup(Db),
    db:close(Db).

setup(Db) ->
    db:clear(Db),
    Rules = rules(),
    addRules(Db, Rules),
    Db.
