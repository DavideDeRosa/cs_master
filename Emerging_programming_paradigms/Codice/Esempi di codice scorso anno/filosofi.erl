-module(filosofi).
-export([test/0, filosofo/4, forchette/1]).
 
% Uso:
% c(filosofi).
% filosofi:test()
%
% Implementazione dei filosofi a cena:
%   - ogni filosofo prende due forchette alla volta
%     => assenza di deadlock
%   - la soluzione presenta busy waiting: se le forchette non sono
%     presenti il filosofo ci riprova dopo un tempo casuale.
%     => la soluzione presenta starvation (se il filosofo N non riesce mai
%     a ottenere le forchette perchè i filosofi N-1 e N+1 si alternano
%     malignamente a mangiare)
%   - notate come la funzione forchette sia tail ricorsiva
%   - usando list comprehension (come nel file filosofi2) è possibile
%     rendere il codice più compatto

sleep(T) ->
        receive
        after T -> true
	end.

% bad: busy waiting
raccogli(F,I) ->
        F ! {raccogli, I, (I + 1) rem 5, self()},
        receive
                ok -> ok ;
                ko ->
                        io:format("no forchette per ~p~n", [I]),
                        sleep(rand:uniform(100)),
                        raccogli(F,I)
        end.

filosofo(I,R,_,0) ->
        io:format("filosofo ~p se ne va~n", [I]),
        R ! {"finito!", I} ;

filosofo(I,R,F,N) ->
        io:format("filosofo ~p pensa~n", [I]),
        sleep(rand:uniform(1000)),
        io:format("filosofo ~p ha fame~n", [I]),
        raccogli(F,I),
        io:format("filosofo ~p mangia~n", [I]),
        sleep(rand:uniform(1000)),
        F ! {rilascia, I, (I + 1) rem 5},
        filosofo(I,R,F,N - 1).

forchette(L) ->
        io:format("forchette disponibili ~p~n", [L]),
        receive
                {raccogli, I, J, PID} ->
                        case lists:member(I,L) andalso lists:member(J,L) of
                                true ->
                                        PID ! ok,
                                        forchette(L -- [I,J]) ;
                                false ->
                                        PID ! ko,
                                        forchette(L)
                        end ;
                {rilascia, I, J} ->
                        forchette(L ++ [I,J])
        end.

test(0,_) -> ok ;
test(N,F) ->
        spawn_link(?MODULE, filosofo, [N - 1,self(),F,20]),
        test(N - 1,F).

waitall(0) -> io:format("ristorante chiuso~n");

waitall(N) ->
        receive
                {"finito!", I} ->
                        io:format("~p andato, ~p mancanti~n", [I, N - 1]),
                        waitall(N - 1) %;
                %X -> io:format("ahi ahi ~p~n",[X]),
                %     waitall(N)
        end.

test() ->
        %process_flag(trap_exit, true),
        F = spawn_link(?MODULE, forchette, [[0,1,2,3,4]]),
        test(5, F),
        %sleep(rand:uniform(3000)),
        %exit(F, bang),
        waitall(5).
