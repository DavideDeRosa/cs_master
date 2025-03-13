-module(qsort).
-export([main/0, qsort/1, psort/1]).

qsort([]) -> [] ;
qsort([H|L]) -> 
    L1 = [ X || X <- L, X =< H ],
    L2 = [ X || X <- L, X > H ],
    qsort(L1) ++ [ H | qsort(L2) ].

psort([]) -> [] ;
psort([H|L]) ->
    L1 = [ X || X <- L, X =< H ],
    L2 = [ X || X <- L, X > H ],
    SELF = self(),
    REF = make_ref(),
    spawn(fun() -> SELF ! {REF, psort(L2)} end),
    psort(L1) ++ [ H | receive {REF, SL2} -> SL2 end].

benchmark(F, L) ->
    T = [ timer:tc(?MODULE, F, L) || _ <- lists:seq(1, 10) ],
    lists:sum([ X || {X, _} <- T ]) / (1000 * length(T)).

main() -> 
    L = [ rand:uniform(10000) || _ <- lists:seq(1, 10000) ],
    io:format("Sequenziale: ~p~n", [ benchmark(qsort, [L]) ]),
    erlang:garbage_collect(),
    io:format("Psort: ~p~n", [ benchmark(psort, [L]) ]),
    erlang:garbage_collect().