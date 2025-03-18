-module(hotswap).

%Versione 1
%-export([loop/2, new_loop/2]).

%Versione 1.5
%-export([loop/1, new_loop/1, new_loop/2]).

%Versione 2
-export([loop/1, new_loop/1]).


% Versione 1: due interi e due msg
% loop(N, M) -> 
%    io:format("Versione 1: ~p, ~p~n", [N, M]),
%    receive
%        {add1, X} -> loop(N + X, M) ;
%        {add2, X} -> loop(N, M + X) ;
%        upgrade -> ?MODULE:new_loop(N, M)
%    end.

% new_loop(N, M) -> loop(N, M).


% Versione 1.5: coppia di interi e sia due msg che un msg
% loop({N, M}) -> 
%    io:format("Versione 1.5: ~p, ~p~n", [N, M]),
%    receive
%        {add1, X} -> loop({N + X, M}) ;
%        {add2, X} -> loop({N, M + X}) ;
%        {add, 1, X} -> loop({N + X, M}) ;
%        {add, 2, X} -> loop({N, M + X}) ;
%        upgrade -> ?MODULE:new_loop({N, M})
%    end.

% Invocata dal vecchio codice
%new_loop(N, M) -> loop({N, M}).

% Invocata dal nuovo codice
%new_loop({N, M}) -> loop({N, M}).


% Versione 2: coppia di interi e sia due msg che un msg
loop({N, M}) -> 
    io:format("Versione 2: ~p, ~p~n", [N, M]),
    receive
        {add, 1, X} -> loop({N + X, M}) ;
        {add, 2, X} -> loop({N, M + X}) ;
        upgrade -> ?MODULE:new_loop({N, M})
    end.

new_loop({N, M}) -> loop({N, M}).