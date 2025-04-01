-module(test_open_port).
-export[test/0].

% Uso:
% c(test_open_port).
% test_open_port:test().
%
% Apre una pipe con "bc -i" (calcolatrice interattiva) tramite open_port.
% Di fatto crea un attore che fa da man-in-the-middle con la pipe:
%  - riceve i comandi e li invia sulla pipe
%  - riceve i dati dalla pipe e li invia al processo che ha invocato open_port
% Nell'esempio usiamo la calcolatrice interattiva per calcolare 3+5.

read(P) ->
    receive
            {P, {data, Data}} ->
                io:format("Received: ~p~n",[Data]),
                read(P)
    after 1000 -> ok
    end.

test() ->
        P = open_port({spawn, "bc -i"}, [binary,{line,255}]),
        read(P),
        receive after 2000 -> ok end,
        P ! {self(), {command, <<"3+5\n">>}},
        io:format("Sent~n"),
        read(P),
        receive after 2000 -> ok end,
        P ! {self(), close}.
