-module(record).
-export([test/0]).

-record(foo, { a = 0, b }).

test() ->
        R = #foo{ a = 3, b = 3},
        io:format("1: ~p, 2: ~p~n", [#foo{ b = 1}, R]). 
