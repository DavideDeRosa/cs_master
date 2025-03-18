-module(bstree3).
-export([test/0]).

% Uso: lanciare il top-level erl e poi dare i comandi
% c(bstree3).
% test().

% Alberi di ricerca non vuoti con valori memorizzati nelle chiavi
% T ::= { leaf, Key, Val } | { node, T, Key, T }

% search(T,Key)
%  ritorna il Val associato alla Key nell'alberto T se c'e'
%  altrimenti ritorna ko

search({leaf, Key, Val}, Key) -> Val ;
search({node, Left, Key, _}, K) when K =< Key ->
  search(Left, K) ;
search({node, _, _, Right}, K) ->
  search(Right, K) ; 
search(_,_) -> ko.

% insert(T, Key, Val)
%   ritorna una copia di T in cui ho associato Val a Key
insert(T = {leaf, Key, _}, Key2, Val2) when Key < Key2 ->
 {node, T, Key, {leaf, Key2, Val2}} ;
insert(T = {leaf, _, _}, Key2, Val2) ->
 {node, {leaf, Key2, Val2}, Key2, T} ;
insert({node, Left, Key, Right}, Key2, Val2) when Key < Key2 ->
 {node, Left, Key, insert(Right, Key2, Val2)} ;
insert({node, Left, Key, Right}, Key2, Val2) ->
 {node, insert(Left, Key2, Val2), Key, Right}.

% La F ha tipo A x B -> A dove A e' il tipo dell'output e
% B il tipo degli elementi della lista. X di tipo A e'
% l'elemento neutro per la F.
%
% foldl(F,X,[X1,...,Xk]) = F(...(F(F(X,X1),X2) ..., Xk)
foldl(_,X,[]) -> X ;
foldl(F,X,[Y|L]) -> foldl(F,F(X,Y),L).

% La F ha tipo A x B -> A dove A e' il tipo dell'output e
% B il tipo degli elementi della lista. La G ha tipo B -> A
% e serve a convertire il primo elemento della lista in un A,
% in assenza di un elemento neutro per la F.
%
% foldls(F,G,[X1,...,Xk]) = F(...(F(F(G(X1),X2),X3) ..., Xk)
% foldls(F,G,[]) = ko
folds(F,G,[X|L]) -> foldl(F,G(X),L).

% from_list(L) restituisce l'albero che contiene tutte
% le coppie {Key, Val} della lista L
from_list(L) ->
 folds(
  fun (T,{Key,Val}) -> insert(T,Key,Val) end, 
  fun ({Key,Val}) -> {leaf, Key, Val} end,
  L).

test() ->
 T = from_list([{3, d}, {0, a}, {1, b}, {4, e}, {2, c}, {5, f}]),
 R = search(T, 2),
 io:format("search(~p,~p) = ~p~n", [T,2,R]).
