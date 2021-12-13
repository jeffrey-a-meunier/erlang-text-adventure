-module(utils).
-export([strcat/1, stringify/1]).

strcat([]) ->
    "";
strcat([S | Ss]) ->
    string:concat(S, strcat(Ss)).

stringify(List) ->
    lists:foldl(fun(E, Acc)->utils:strcat([Acc, E, " "]) end, "", List).
