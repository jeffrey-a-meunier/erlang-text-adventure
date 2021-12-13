-module(main).
-export([dbFile/0, start/0]).

-define(FILENAME, "data.db").

dbFile() ->
    ?FILENAME.

start() ->
    FileName = dbFile(),
    case filelib:is_regular(FileName) of
        false ->
            setup:setup();
        _ -> ok
    end,
    {ok, Db} = db:open(FileName),
    io:format("Enter the command 'help' for help~n"),
    location:show(Db),
    run(Db),
    dets:close(Db).

prompt() ->
    "=> ".

run(Db) ->
    Prompt = prompt(),
    Line = string:chomp(io:get_line(Prompt)),
    Parts = parser:parse(Line),
    Res = cmd:handle(Db, Parts),
    case Res of
        ok -> run(Db);
        end_game -> ok;
        quit -> ok
    end.
