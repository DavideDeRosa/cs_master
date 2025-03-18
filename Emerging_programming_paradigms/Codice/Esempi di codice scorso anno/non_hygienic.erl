-module(non_hygienic).
-export([test/0]).

% Uso:
% c(non_hygienic).
% non_hygienic:test()
%
% Il file mostra come il sistema di macro di Erlang, come quello di C, sia
% non igenico: il processo di sostituzione avviene dopo l'analisi lessicale
% e prima del parsing e NON tiene conto della struttura di binding :-(
%
% L'esempio qua sotto definisce la macro foo(X). Per via della non-igenicità,
% ?foo(Z) viene espanso correttamente a fun(Y) -> Z + Y end, mentre
% ?foo(Y) viene espanso erroneamente  a fun(Y) -> Y + Y end, come mostra
% il test qua sotto che stampa 5 e 6 (invece di 5 e 5). 
%
% Elixir, implementato sulla BEAM, fornisce grosso modo le stesse funzionalità
% di Erlang con una sintassi differente. Fra le caratteristiche peculiari di
% Elixir c'è l'uso di un sistema di macro igieniche.

-define(foo(X), fun (Y) -> X + Y end).

test() ->
        Z = 2,
        Y = 2,
        io:format("No capture: ~p, capture: ~p~n", [(?foo(Z)(3)), ?foo(Y)(3)]).
