-module(test_bitstrings).
-export([test/0]).

% Uso:
% c(test_bistrings).
% test_bitstrings:test()
%
% L'esempio mostra il caso più semplice di creazione e patter-match su un bitstring.
% La sintassi ammette moltissime altre varianti.
% I bitstrings (e i binaries, ovvero bitstrings il cui numero di bit è multiplo di 8)
% sono utilizzati per interfacciarsi con l'hardware e per implementare protocolli di
% rete. Per esempio, tramite patter-matching si riesce facilmente a processare un
% pacchetto IP estraendone i vari campi (versione, lunghezza, ...).

test() ->
        X = <<3:2, 1:2>>,         % 3 written in 2 bits, 1 in 2 bits,  i.e 1101
        <<B:1, C/bitstring>> = X,  % B is the first bit, C the remaining,
                                  % i.e. B = 1, C = 101
        io:format("First bit: ~p, remaining bits: ~p~n", [B,C]).
