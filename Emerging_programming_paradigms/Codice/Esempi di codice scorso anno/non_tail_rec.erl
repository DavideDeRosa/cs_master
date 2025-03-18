-module(non_tail_rec).
-export([test_no_tr/1, test_tr/1, test_tr2/1, test_no_tr2/1]).

% Uso:
% c(non_tail_rec).
%
% non_tail_rec:test_no_tr(0).
%       Si vede che l'uso della memoria aumenta a ogni chiamata
%       ricorsiva per via degli stack frame: la chiamata non
%       è di coda
%
% non_tail_rec:test_tr(0).
%       Questa volta la chiamata è di coda e l'uso di memoria
%       rimane costante.
%
% non_tail_rec:test_tr2(0).
%       Questo codice è equivalente al precedente, ma esteticamente
%       più piacevole. Dopo l'of si può descrivere (tramite pattern
%       matching) cosa fare del risultato. In questo esempio si
%       procede sempre comunque alla chiamata ricorsiva. Il codice
%       nell'of NON è protetto dal catch (ovvero le eccezioni sollevate
%       in tale ramo scappano). Pertanto prima di valutare l'of si può
%       immediatamente fare pop del catch frame dallo stack. Il codice
%       after (se presente) viene comunque eseguito.
%
% non_tail_rec:test_no_tr2(0).
%       Nonostante il codice usi l'of come nel caso test_tr2, c'è una
%       clausola after che deve essere eseguita dopo il ramo of. Pertanto
%       la chiamata ricorsiva non è di coda e la memoria cresce.

update(_State) -> ok. % qua posso sollevare eccezione

test_no_tr(State) ->
        io:format("memory used no_tr: ~p~n", [erlang:memory(total)]),
        try test_no_tr(update(State))
        catch _ -> ko
        end.

test_tr(State) ->
        io:format("memory used tr: ~p~n", [erlang:memory(total)]),
        test_tr(
                try update(State)
                catch _ -> ko
                end).

test_tr2(State) ->
        io:format("memory used tr: ~p~n", [erlang:memory(total)]),
        try update(State) of
                UpdatedState -> test_tr2(UpdatedState)
        catch _ -> ko
        end.

test_no_tr2(State) ->
        io:format("memory used tr: ~p~n", [erlang:memory(total)]),
        try update(State) of
                UpdatedState -> test_no_tr2(UpdatedState)
        catch _ -> ko
        after ok
        end.
