-module(eccezioni).
-export([test/0]).

% albero non ordinato
% T ::= leaf | {node, T, key, value, T}

% find che non usa eccezioni
% find(Key, T) = none | {some, Value}

find(_, leaf) -> none ;
find(Key, {node, _, Key2, Value, _}) when Key =:= Key2 ->
  {some, Value} ;
find(Key, {node, L, _, _, R}) ->
  case find(Key, L) of
    none -> find(Key, R) ;
    x -> x
  end.

% finde che usa eccezioni come control operator per
%   cortocircuitare tutti i case una volta trovato
%   il valore nell'albero
% finde(Key, T) = none | {some, Value}

findaux(_, leaf) -> none ;
findaux(Key, {node, _, Key2, Value, _}) when Key =:= Key2 -> throw({found, Value});
findaux(Key, {node, L, _, _, R}) ->
  findaux(Key, L),
  findaux(Key, R).

finde(Key, T) ->
  try
   findaux(Key, T)
  catch
   {found, Value} -> {some, Value}
  end.

test() ->
 T = {node, {node, leaf, 1, 1, leaf},
      2, 2, {node, leaf, 3, 3, leaf}},
 X = find(3,T),
 Y = finde(3,T),
 X =:= Y.
