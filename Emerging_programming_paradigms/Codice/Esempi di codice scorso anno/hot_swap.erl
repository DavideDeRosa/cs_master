-module(hot_swap).
-export([test/1, update_test_data/1]).

% Uso:
% c(hot_swap).
% P = spawn(hot_swap, test, 0).
% P ! {add, 4}.
% P ! {add, 2}.
% Commentare via la prima versione del codice
% e scommentare la seconda.
% c(hot_swap).
% P ! update.
% P ! {add, 4}.

%%%% PRIMA VERSIONE DEL CODICE %%%%
test(N) ->
       io:format("(~p) N = ~p~n", [self(), N]), 
       receive
                {add, M} -> test(N + M) ;
                update ->
                        N2 = ?MODULE:update_test_data(N),
                        io:format("~p~n", [N2]),
                        ?MODULE:test(N2)
       end.

update_test_data(N) -> N.

 
%%%% SECONDA VERSIONE DEL CODICE %%%%
%test(N) ->
%       {N1, N2} = N,
%       io:format("(~p) N1 = ~p, N2 = ~p~n", [self(), N1, N2]), 
%       receive
%                {add, M} -> test({N1 + M, N2 - M}) ;
%                update ->
%                        N3 = update_test_data(N),
%                        ?MODULE:test(N3)
%       end.
%
%update_test_data(N) -> {N, N}.
