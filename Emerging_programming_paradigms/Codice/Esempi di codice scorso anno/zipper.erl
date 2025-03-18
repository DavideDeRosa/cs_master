-module(zipper).
-export([]).

% Zipper = cercare di avere UNO E UN SOLO
% "puntatore" all'interno di un albero (lista come
% caso particolare) e poter fare
% - modifiche locali ("mutazioni", inserimenti, cancell.)
% - movimenti locali
% Dove ogni operazione deve essere O(1) in tempo e in
% spazio

% Esempio: uno zipper su una lista
% [] <- 1 <- 2 <- 3 <- 4   5 -> 6 -> 7 -> 8 -> []
%                      ^   ^

% Zipper = una coppia di liste {P,N}

% crea zipper vuoto
empty() -> {[],[]}.

% fa push dopo il puntatore
push(X,{P,N}) -> {P,[X|N]}.

% fa push prima del puntatore
push2(X,{P,N}) -> {[X|P],N}.

% pop dopo il puntatore; restituisce il nuovo zipper
% e l'elemento popped
pop({_,[]}) -> none ;
pop({P,[X|N]}) -> {some, {{P,N}, X}}.
 
% pop prima del puntatore; restituisce il nuovo zipper
% e l'elemento popped
pop2({[],N}) -> none ;
pop2({[X|P],N}) -> {some, {{P,N}, X}}.

% next muove il puntatore all'avanti
next({_,[]}) -> none ;
next({P,[X|N]}) -> {some, {[X|P],N}}.
 
% prev muove il puntatore all'indietro
prev({[],_}) -> none ;
prev({[X|P],N}) -> {some, {P,[X|N]}}.

%%%%%%%%%%%%%%%%%%%%%%
 
% Esempio: uno zipper su un albero binario
%            X
%            .
%            |
%           7.5
%           / .
%          .   \
%         X      8  <- P
%               .  \
%              /    .
%       N -> 6      10
%          /  \    /  \
%         .    .   .   .
%        4      7  X   X
%       / \    / \
%      .   .  .   .
%     X     X X   X
%
%  Zipper = {N,P}
%   N punta in basso ed è un albero
%   P punta verso l'alto ed è un
%    P = {from_left, T, P} | {from_right, T, P} | root
%    o equivalentemente
%      P = una lista di {from_left,T} o {from_right,T}
%    o P = una lista di {is_from_left,T}
%                        booleano
%


% Un tipo di dato algebrico ha come possibili forme
%  - atomi es. a | b | c | []
%  - tuple taggate es. {from_left, T}, {node, L, A, R},
%     [X|L]
%
% Li posso tutti pensare come dei POLINOMI
%
% Es. liste   L = [] | [N|L]
%                 nil | {cell, N, L}
%
% L = 1 + Nat x L     è un polinomio!
%
% 1 (o unit) è un tipo che contiene un solo valore (es. nil)
% 0 (o void/null) è un tipo che non contiene valori
% + = unione disgiunta fra tipi
% x = prodotto cartesiano di tipi
%
%
% Es. alberi binari
%          T = nil | {cell, T, N, T}
%
% T = 1 + T x nat x T    è un polinomio!
%
%
% Derivata = zipper
%
% indico con F' la derivata rispetto a nat
%
% L' = (1 + nat x L)'
%    = 1' + (nat x L)'
%    = 0 + nat' x L + nat x L'
%    =        1 x L + nat x L'
%    =            L + nat x L'
%           lista in avanti  puntatori all'indietro
%
% T' = (1 + T x nat x T)'
%    = 1' + (T x nat x T)'
%    = 0  + T' x nat x T + T x 1 x T + T x nat x T'
%    =    T x T     + T' x nat x T + T x nat x T'
%    albero in giù    --------------------------
%                      i due casi con un punt. in su
%                      e uno in giù
