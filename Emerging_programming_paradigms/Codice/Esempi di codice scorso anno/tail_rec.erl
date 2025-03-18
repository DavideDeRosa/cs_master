-module(tail_rec).
-export([main/1]).

% Use:
% c(tail_rec).
% tail_rec:main(5000000).
%
% The file calls a tail-recursive foldl, a tail-recursive foldr
% and finally a non tail-recursive foldr to compute the product of
% the elements in a list of 1s.
%
% The program shows the amount of memory used by the three implementations:
% the tail-recursive foldl uses much less memory than the other two because
% the stack does not grow. The tail-recursive foldr builds a new list in
% memory and therefore uses more memory than the foldl. The non tail-recursive
% foldr is, however, the worst for memory usage.
%
% Erlang does not stack overflow even on very large lists because the BEAM
% virtual machine dynamically grows the stack. All functions eventually exhaust
% the heap on very large inputs.

% TAIL RECURSIVE
% foldl(F,A,[X1,...,Xk]) = F(...F(A,X1) ..., XK)
foldl(_F,A,[]) ->
        io:format("FOLDL memory: ~p~n", [erlang:memory(total)]),
        A ;
foldl(F,A,[H|TL]) -> foldl(F,F(A,H),TL).

% NON TAIL RECURSIVE
% foldr(F,A,[X1,...,Xk]) = F(...F(A,Xk) ..., X1)
foldr(_F,A,[]) ->
        io:format("FOLDR memory: ~p~n", [erlang:memory(total)]),
        A ;
foldr(F,A,[H|TL]) -> F(H,foldr(F,A,TL)).

% reverse([X1,...,Xk]) = [Xk,....,X1]
reverse(L) -> reverse(L,[]).

reverse([],A) -> A ;
reverse([H|TL],A) -> reverse(TL,[H|A]).

% TAIL RECURSIVE
% foldrtr(F,A,[X1,...,Xk]) = F(...F(A,Xk) ..., X1)
foldrtr(F,A,L) -> foldl(F,A,reverse(L)).

gen(0,A) -> A ;
gen(N,A) -> gen(N-1, [1|A]).

main(N) ->
        L = gen(N,[]),
        RL = foldl(fun (X,Y) -> X * Y end, 1, L),
        io:format("FOLDL:   ~p~n", [RL]),
        garbage_collect(), % Forces a major garbage collection
        RRTL = foldrtr(fun (X,Y) -> X * Y end, 1, L),
        io:format("FOLDRTL: ~p~n", [RRTL]),
        garbage_collect(), % Forces a major garbage collection
        RR = foldr(fun (X,Y) -> X * Y end, 1, L),
        io:format("FOLDR:   ~p~n", [RR]).
