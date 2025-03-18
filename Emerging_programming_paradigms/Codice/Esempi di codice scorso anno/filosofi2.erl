-module(filosofi2).
-export([main/0,tavolo/2,philosopher/4]).

% Uso:
% c(filosofi2).
% filosofi2:main()
%
% Implementazione dei filosofi a cena:
%   - ogni filosofo prende una forchetta alla volta
%     => possibilità di deadlock
%   - la soluzione non presenta busy waiting.
%     Per evitare il busy waiting il tavolo mantiene nella lista Pending
%     le richieste di forchette già estratte dalla mailbox e non ancora
%     processate. Esse vengono processate come le richieste nella mailbox
%     e con precedenza maggiore rispetto a quest'ultime.
%   - la soluzione non presenta starvation
%   - notate come la funzione tavolo sia tail ricorsiva

tavolo(L,Pending) ->
        Process_pending =
           fun
                Aux([]) -> ko ;
                Aux([T = {get, Phil, X}|Tl]) ->
                  case lists:member(X,L) of
                     true ->
                        Phil ! ok,
                        {ok, L -- [X], Pending -- [T]} ;
                     false -> Aux(Tl)
                  end
           end,
        case Process_pending(Pending) of
                {ok, L2, Pending2} -> tavolo(L2,Pending2) ;
                ko ->
                 io:format("tavolo fa receive, forchette:  ~p~n",[L]),
                 receive
                         T = {get, Phil, X} ->
                                 case lists:member(X,L) of
                                         true ->
                                                 Phil ! ok,
                                                 tavolo(L -- [X], Pending) ;
                                         false -> tavolo(L, [T|Pending])
                                 end ;
                         {free, X} ->
                                 tavolo([X|L], Pending)
                 end
        end.

get_fork(N, Table, X) ->
        Table ! {get, self(), X},
        io:format("~p attende forchetta ~p~n",[N,X]),
        receive ok -> ok end,
        io:format("~p ha preso forchetta ~p~n",[N,X]).

release_fork(Table,X) ->
        Table ! {free, X}.

sleep(N) -> receive after N*10 -> ok end.

philosopher(Main,Table,N,Iteration) when Iteration > 0 ->
        io:format("~p thinks~n", [N]),
        sleep(rand:uniform(3)),
        io:format("~p is hungry~n", [N]),
        get_fork(N,Table,N),
        sleep(rand:uniform(2)),
        get_fork(N,Table,(N+1) rem 5),
        io:format("~p eats~n", [N]),
        sleep(rand:uniform(2)),
        release_fork(Table,N),
        release_fork(Table,(N+1) rem 5),
        philosopher(Main,Table,N,Iteration - 1) ;
philosopher(Main,_,_,0) -> Main ! exit.

main() ->
        SEQ = lists:seq(0,4),
        Table = spawn(?MODULE,tavolo,[SEQ, []]),
        [ spawn(?MODULE,philosopher,[self(),Table,Phil,5]) || Phil <- SEQ],
        [ receive exit -> io:format("Bye bye~n") end || _ <- SEQ ].
