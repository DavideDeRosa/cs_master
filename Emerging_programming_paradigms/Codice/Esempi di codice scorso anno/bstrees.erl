-module(bstrees).
-export([test/0]).

% Uso:
% c(bstrees).
% bstrees:test().
%
% Mostra l'uso di Erlang come linguaggio di programmazione funzionale puro
% ovvero higher-order (= funzioni come oggetti di prima classe) e privo di
% mutabilità.

% Un Binary Search Tree è o vuoto (nil) oppure un nodo {node, K, L, R} dove
% K è la chiave e L,R i due sotto-alberi sinistro e destro.

% empty ritorna un albero vuoto
empty() -> nil.

% insert(T,K) inserisce la chiave K nell'albero T
insert(nil,K) -> {node, K, nil, nil} ;
insert({node, K1, L, R}, K) when K < K1 ->
       % l'intero sotto-albero destro R viene condiviso in memoria (è shared)
       % con l'albero prima dell'inserzione, che non viene modificato e non
       % viene distrutto (garbage collected) fino a quando è ancora in uso.
       {node, K1, insert(L,K), R} ;
insert({node, K1, L, R}, K) when K > K1 ->
       {node, K1, L, insert(R,K)} ;
insert(N, _K) -> N.
% Poichè i rami non modificati sono sharati con l'albero in input, inserire
% un nodo alloca solo k nuove tuple dove k è il numero di chiamate ricorsive,
% ovvero la lunghezza del cammino radice-punto dove k viene inserito.
% Se l'albero fosse bilanciato, questo sarebbe O(log N) per N numero dei nodi.

% foldl(F,ACC,[N1,...,Nk]) (copia di lists:foldl)
% restituisce F(...F(F(ACC,N1),N2)...,Nk)
% La l in foldl significa da sinistra a destra ed è l'ordine di visita
% degli elementi nella lista.
% foldl è una funzione higher-order perchè prende un'altra funzione in input
foldl(_F,R,[]) -> R ;
foldl(F,R,[N|L]) -> foldl(F,F(R,N),L).

% Costruisce l'albero binario di ricerca ottenuto inserendo consecutivamente
% i numeri nella lista a partire dall'albero vuoto.
% (fun insert/2) è la (reference alla) funzione insert con due parametri
% se scrivessi solo insert verrebbe confuso con l'atomo insert :-(
test() -> foldl(fun insert/2, empty(), [3,1,3,4,0,2,7]).
