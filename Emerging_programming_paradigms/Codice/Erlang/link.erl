-module(link).
-export([sleep/1, make_link/2, test/0]).

% link è:
%       - bidirezionale
%       - idempotente
%
% Tipico esempio d'uso della link:
%   dati due attori, il corrente e PID,
%   il corrente vuole richiedere un servizio a PID
%   dal quale vuola una risposta. Come fa?
%   Un possibile esempio può essere:

sync_call(REQUEST, PID, TIMEOUT) -> 
    REF = make_ref(),
    link(PID),
    PID ! {REF, REQUEST},

    RES = 
        receive 
            {REF, ANSWER} -> ANSWER 
        after TIMEOUT -> exit(timeout) 
        end,

    unlink(PID),
    RES.


% monitor è
%       - unidirezionale
%       - non idempotente
%       - non transitiva
%
% Esempio 1: un browser monitora (non linka) un server web
%
% Esempio 2:
%       - uso due librerie B e C
%       - sia B che C usano una libreria A
%       - tutte le librerie sono implementate come attori
%
% Link vs monitor (unlink vs demonitor):
%       - link e monitor prendono il PID dell'altro attore
%       - link non ritorna nulla, monitor ritorna un hanlde
%       - unlink prende il PID, demonitor prende l'handle e il PID
%       - trap_link funziona solo con link
%       - i messaggi legati al monitoring sono sempre in user space e non vengono propagati
%
% Variante per i sistemi distribuiti -> è possibile fare monitoring di un NODO.


% ------------------------------------------------------------------------ %
% Codice make_link

sleep(N) -> receive after N -> ok end.

make_link(SHELL, 0) ->
    io:format("0: send to SHELL ~p~n", [SHELL]),
    _ = 1 / 0,
    SHELL ! self(),
    sleep(200000),
    io:format("0: Termino ~n"),
    ok;
make_link(SHELL, N) -> 
    process_flag(trap_exit, true),
    spawn_link(?MODULE, make_link, [SHELL, N - 1]),
    sleep(6000),
    receive
        MSG -> io:format("~p: Ho ricevuto ~p~n", [N, MSG])
    after 1000 -> ok
    end,
    io:format("~p: Termino ~n", [N]).

% Creo una catena di 10 attori ognuno linkato al precedente
test() -> 
    spawn(?MODULE, make_link, [self(), 10]),

    receive
        PID ->
            io:format("Shell ha ricevuto da ~p~n", [PID]),
            exit(PID, kill),
            ok
    end.