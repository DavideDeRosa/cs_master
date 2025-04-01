-module(mailbox).
-export([main/0,loop/3,test_receiver/1]).

% Uso:
%
% c(mailbox).
% mailbox:main()
%
% Implementazione di una mailbox bounded.
% send(Mailbox,Msg) invia il Msg alla Mailbox, bloccante se piena.
% full(Mailbox) ritorna true sse la Mailbox è piena
% consumed(Mailbox) da chiamarsi dopo aver estratto un messaggio dalla mailbox
%                   (ovvero dopo aver fatto una receive)
%
% send/full devono essere chiamate da chi vuole inviare un messaggio
% chi vuole riceverlo, ovvero associarsi alla mailbox, deve mandare alla
% mailbox il messaggio {setreceiver, PID}
%
% loop è il main loop di un processo mailbox che funge da man-in-the-middle
% fra i mittenti e il ricevente.

%% Mittente

send(Dest, Msg) ->
        Dest ! {space, self()},
        receive
                ok -> Dest ! { msg, Msg }
        end.

full(Dest) ->
        Dest ! {full, self()},
        receive
                R -> R
        end.

consumed(Mailbox) ->
        Mailbox ! received.

% È l'utente che deve inserire una chiamata a
% consumed in ogni ramo di una receive
%receive2([p1,f1, ..., pn,fn]) ->
        %receive
                %p1 -> consumed(Mailbox), f1() ;
                %p2 -> consumed(Mailbox), f2()

%% Ricevente

% N = numero di messaggi nella mailbox
% C = capacità della mailbox
% PID = PID del proprietario della mailbox
loop(N,C,PID) ->
        receive
                {space, Sender} when N < C ->
                        Sender ! ok,
                        loop(N+1,C,PID) ;
                {full, Sender} ->
                        Sender ! (N > C),
                        loop(N,C,PID) ;
                {msg, Msg} when PID =/= unknown ->
                        PID ! Msg,
                        loop(N,C,PID) ;
                received ->
                        loop(N-1,C,PID) ;
                {setreceiver, Receiver} ->
                        loop(N,C,Receiver)
        end.

sleep(N) ->
        receive
        after N*1000 -> ok
        end.

test_receiver(Mailbox) ->
        Mailbox ! {setreceiver, self()},
        io:format("Receiver goes to sleep~n"),
        sleep(10),
        F = fun Loop() ->
                receive X ->
                        consumed(Mailbox), % Ugly and error prone!
                        io:format("Ricevuto ~p~n", [X]),
                        Loop()
                end
            end,  
        F().

test_client(Mailbox) ->
        io:format("Invio~n"),
        send(Mailbox, uno),
        io:format("Invio~n"),
        send(Mailbox, due),
        io:format("Invio~n"),
        send(Mailbox, tre),
        io:format("Invio~n"),
        send(Mailbox, quattro),
        io:format("Invio~n"),
        send(Mailbox, cinque),
        io:format("Invio~n"),
        send(Mailbox, sei).

main() ->
        Mailbox = spawn(?MODULE,loop,[0,4,unknown]),
        spawn(?MODULE,test_receiver,[Mailbox]),
        test_client(Mailbox),
        ok.
