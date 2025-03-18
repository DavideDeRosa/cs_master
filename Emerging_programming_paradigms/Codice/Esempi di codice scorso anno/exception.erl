-module(exception).
-export([f/1]).

% Esempio di codice con un pattern per catturare insiemi di
% eccezioni. L'esempio mostra come ad ogni chiamata ricorsiva
% le variabili N e X siano fresche (sono allocate su un nuovo
% stack frame!) e pertanto il match leghi la prima volta la
% prima X a uno e la seconda a due.

f(N) ->
        try throw({errorio, N})
        catch
                {errorio, X} ->
                        io:format("CATTURATA ~p~n", [X]),
                        case X of
                                uno -> f(due) ;
                                _ -> ok
                        end
        end.
