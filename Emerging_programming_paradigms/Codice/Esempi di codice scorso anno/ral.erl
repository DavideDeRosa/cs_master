-module(ral).
-export([]).

% Tipo di dato imperativo con:
% - push e pop in O(1)
% - accesso all'i-esimo elemento in O(1)
% - modifica dell'i-esimo elemento in O(1)
% Array funziona con costi precedenti AMMORTIZZATI

% Okasaki: Random Access Lists (RAL)
% - push e pop in O(1)
% - accesso/modifica dell'i-esimo elemento in O(log N)
% dove N è il numero di elementi
% con costi NON ammortizzati


% Idea: partiamo da un albero binario COMPLETO
% COMPLETO = tutti i livelli dell'abero sono pieni
% Un albero completo di altezza 2^n-1 ha ?? nodi
% Dato un albero completo, considerate la sua in-visita
%
%                  4
%             2            6(2)
%         1      3    5(1)     7(3)
%
% Dato un albero completo con N nodi, se cerco la posizione% M:
% - se M > (N+1)/2 la trovo nel sottoalbero di dx
%   in posizione M - (N+1)/2
% - se M < (N+1)/2 la trovo nel sottoalbero di sx
% - se M = (N+1)/2 la radice è il nodo che cerco
%
%
% Qualunque numero n può essere rappresentato dalla
% somma di potenze del 2 meno 1 dove la potenza più
% bassa può avere molteplicità 1 o 2
%
%
% Es: vogliamo rappresentare 6 elementi
%
% 6 = 2^2-1 + 2^2-1
% 7 = 2^3-1
% 8 = 2^1-1 + 2^3-1
% 9 = 2^1-1 + 2^1-1 + 2^3-1
% 10 = 2^2-1 + 2^3-1
%
% RAL = una lista di coppie albero completo/dimensione
%       ordinata per dimensioni crescenti con l'eccezione
%       che le prima due dimensioni possono essere uguali
%
%
% Es: 9 elementi
%
%
%  1: 1     ---   1: 2    --- 7:   6  ----  []
%                                 / \
%                                4   8
%                               /\   /\
%                              3  5 7  9
%
%
% Push nel caso in cui NON ho i primi due alberi di
% dimensione 1: aggiungo in testa alla lista un albero
% singoletto
%
% Push(Z) nel caso in cui i primi due alberi hanno
% dimensione % 1:
%
% da       1: X  --  1: Y -- L
% ottengo  3:   X         -- L
%             /   \
%           Z       Y
