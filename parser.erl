-module(parser).
-export([parse/1]).

parse(String) ->
    Parts = string:lexemes(String, " "),
    Parts1 = case Parts of
                 [Verb] ->
                     #{verb => Verb, object => none};
                 [Verb, Object] ->
                     #{verb => Verb, object => Object}
             end,
    Parts1.
