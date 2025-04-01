-module(test_receive).
-export([test/0, g/0]).

% Uso:
% c(test_receive).
% test_receive:test().
%
% Cosa mostra: 
% - mentre il processo P dorme, riceve in ordine i tre messaggi b, c, a
% - al suo risveglio itera una receive con primo pattern a e secondo b
% - il test mostra che la receive itera sulla coda di messaggi e per ogni
%   messaggio itera sui pattern alla ricerca del primo che fa match; se
%   nessuno fa match passa al messaggio successivo nella coda o sospende
%   l'attore se tutti i messaggi nella coda sono stati analizzati

sleep(N) -> receive after N -> ok end.

f() ->
        io:format("f ready~n") ,
        receive
                a -> io:format("~p~n",[a]) ;
                b -> io:format("~p~n",[b])
        end ,
        f().

g() -> sleep(5000), f().

test() ->
        P = spawn(fun g/0),
        P ! b,
        P ! c,
        P ! a,
        io:format("messaggi mandati~n"),
        sleep(20000).
