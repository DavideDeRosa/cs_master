-module(link2).
-export([main/0]).

%%%% Implementazione di message passing sincrono con gestione dei fallimenti %%%%
%%%% Ricevente e mittente devono morire entrambi in caso di fallimento di uno dei due %%%

sync_receive() ->
  receive {Req, Pid, Ref} ->
    % calcolare la risposta
    Pid ! { answer, Ref }
  end.

% implementazione safe della chiamata sincrona a un attore che sta collaborando con me
sync_call(Pid,Msg) ->
  Ref = make_ref(),
  Pid ! {Msg, self(), Ref},
  link(Pid),
  % o mi lascio morire oppure con trap_exit gestisco la morte del chiamato
  receive {Ans, Ref} -> Ans end,
  unlink(Pid).

%%%% Richiesta asincrona a un server lento con recupero della risposta e gestione del fallimento del server %%%
%%%% Il server non deve morire se muore il client %%%
my_code(Server) ->
  %%%%
  Ref = make_ref(),
  Server ! {request, self(), Ref },
  Uid = monitor(process,Server),
  %%% continuo a lavorare (e potenzialmente posso crashare)
  %%% ora mi serve proprio la risposta
  X = receive {Ans, Ref} -> Ans end,
  demonitor(Uid)
  %%% termino la computazione
  .

%%%% Creazione di un attore per parallelizzare una soluzione %%%
%%%% I due attori devono morire entrambi nel caso di fallimento di uno dei due %%%
%%%% Attenzione alla race condition se usa spawn seguito da link invece della spawn_link %%%
%%%% Esiste anche la spawn_monitor %%%%

f() ->
  Pid = self(),
  Ref = make_ref(),
  P = spawn_link(fun() -> % faceva qualcosa potendo crashare
                 Pid ! {answer, Ref} end),
  %%% RACE CONDITION QUI!!!
  %link(P),
  % la spawn_link evita la race condition
  % faccio altro
  receive {Answer, Ref} -> something end
.

main() -> ok.
