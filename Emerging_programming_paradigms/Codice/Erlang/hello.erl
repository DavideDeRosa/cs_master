-module(hello).
-export([cc/1]).

cc(Bal) -> 
    receive
        print -> io:format("Il balance e' ~p ~n", [Bal]), cc(Bal);
        {put, N} -> cc(Bal + N);
        {get, PID} -> PID ! Bal, cc(Bal);
        exit -> ok
    end.