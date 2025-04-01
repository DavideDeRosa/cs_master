-module(hello_world).
-export([main/1]).

% Uso:
% c(hello_world).
% P = spawn(hello_world, main, [0]).
% P ! {add, 2}.
% P ! {add, 4}.

main(N) ->
        io:format("(~p) N=~p~n", [self(),N]),
        receive
                { add, M } ->
                        main(N + M)
        end.
