-module(qsort).
-export([main/0, qsort/1, psort/1, psort2/2, jsort/1, scheduler/1, worker/0]).

% Funzione che implementa l'algoritmo QuickSort
% (differente dal solito qsort studiato, questa è una versione funzionale non in-place).
% Viene utilizzata la list comprehension per dividere la lista in due parti.
% Viene quindi richiamata la funzione in maniera ricorsiva per riordinare le due liste ottenute.
% Infine le liste vengono concatenate, ottenendo la lista ordinata.
qsort([]) -> [] ;
qsort([H|L]) -> 
    L1 = [ X || X <- L, X =< H ],
    L2 = [ X || X <- L, X > H ],
    qsort(L1) ++ [ H | qsort(L2) ].


% Prima versione per parallelizzare il processo di sorting della lista.
% Viene divisa la lista come in precedenza, per poi spawnare un nuovo attore
% che si occuperà di eseguire la funzione in maniera ricorsiva sulla parte destra 
% della lista. A sua volta l'attore spawnerà nuovi attori per completare l'esecuzione. 
% Alla fine vengono concatenate le diverse liste per ottenere la lista ordinata.
psort([]) -> [] ;
psort([H|L]) ->
    L1 = [ X || X <- L, X =< H ],
    L2 = [ X || X <- L, X > H ],
    SELF = self(),
    REF = make_ref(),
    spawn(fun() -> SELF ! {REF, psort(L2)} end),                % L'attore spawnato invia il messaggio contenete la lista ordinata al padre.
    psort(L1) ++ [ H | receive {REF, SL2} -> SL2 end].          % Il padre resta in attesa della lista. Il REF viene usato per il pattern matching della msg queue.


% Seconda versione del psort, che prende in input della funzione un numero N 
% che limita il numero attori creati. Il valore di N viene decrementato ad ogni
% chiamata ricorsiva, in modo che man mano che vengono spawnati nuovi attori il 
% valore di N tenderà ad arrivare a 0, eseguendo quindi l'algoritmo sequenziale.
psort2(0, L) -> qsort(L) ;                                      % Se N = 0, si effettua un qsort sequenziale per evitare overhead.
psort2(_, L) when length(L) =< 10 -> qsort(L) ;                 % Se la lista è troppo piccola, si effettua un qsort sequenziale per evitare overhead.
psort2(N, [H|L]) ->
    L1 = [ X || X <- L, X =< H ],
    L2 = [ X || X <- L, X > H ],
    SELF = self(),
    REF = make_ref(),
    spawn(fun() -> SELF ! {REF, psort2(N - 1, L2)} end),        % L'attore spawnato invia il messaggio contenete la lista ordinata al padre.
    SL1 = psort2(N - 1, L1),
    SL2 = receive {REF, RES} -> RES end,                        % Il padre resta in attesa della lista. Il REF viene usato per il pattern matching della msg queue.
    SL1 ++ [ H | SL2].


% Implementazione dell'attore scheduler, il quale ha lo scopo di distribuire
% i diversi job ai vari worker. Lo scheduler può ricevere nuovi job dagli attori
% (salvandoli nella propria lista di jobs interna) o ricevere richieste GET da parte
% dei diversi worker, i quali richiedono un nuovo job da eseguire.
scheduler(JOBS) -> 
    receive
        {require, _, _, _} = JOB -> scheduler([JOB|JOBS]) ;
        {get, REF, WORKER} when JOBS =/= [] ->                  % Viene inserita una guardia che controlla se la lista di jobs è vuota. Se vuota non viene fatto pattern matching
            [JOB|L] = JOBS,                                     % Viene divisa la lista JOBS in JOB (primo elemento) e L (il resto della lista) tramite pattern matching refutabile.
            WORKER ! {REF, JOB},                                % Al worker viene quindi inviato il primo elemento (JOB).
            scheduler(L)                                        % Lo scheduler viene eseguito ricorsivamente sul resto della lista (L).
    end.


% Implementazione dell'attore worker, il quale ha lo scopo di eseguire
% jobs ricevuti dallo scheduler e di inviare il risultato di queste
% esecuzioni all'attore che ha effettuato la richiesta, senza passare
% dallo scheduler. Per comunicare con lo scheduler viene utilizzato il
% costrutto register(), il quale permette di utilizzare l'atomo "scheduler"
% per comunicare con l'attore designato all'esecuzione dello scheduler.
worker() ->
    REF = make_ref(),
    scheduler ! {get, REF, self()},                             % Il worker richiede nuovi job allo scheduler.
    receive 
        {REF, {require, PID, REF2, F}} ->                       % Il worker resta in attesa di nuovi job dallo scheduler. Quando il pattern fa match viene restituito
            PID ! {REF2, F()},                                  % il risultato all'attore che ha inviato inizialmente la richiesta di job allo scheduler.
            worker()                                            % Il worker richiama ricorsivamente se stesso per richiedere nuovi job allo scheduler.
    end.


% Terza versione, con l'idea di base di fare eseguire dei job ad una pool di worker.
% I worker ottengono i diversi job da uno scheduler, per poi inviare il risultato
% della propria esecuzione direttamente all'attore che lo ha richiesto allo scheduler.
% Viene quindi concatenato il risultato con tutti i risultati ottenuti dai worker.
%
% NOTA BENE: questo codice non funziona, va in deadlock. Il deadlock viene causato dal
% fatto che tutti i worker restano in attesa dei risultati da calcolare dagli altri worker,
% fino al non avere più worker disponibili. Questa problematica si può risolvere controllando
% il numero di worker disponibile prima di continuare. Se il numero è vicino al numero di worker
% disponibili, si ripiega sull'algoritmo sequenziale. 
% (Per ulteriori dettagli vedere Virtuale anno precedente, dove è presente codice completo fixato).
jsort([]) -> [] ;
jsort(L) when length(L) =< 10 -> qsort(L) ;                     % Se la lista è troppo piccola, si effettua un qsort sequenziale per evitare overhead.
jsort([H|L]) ->
    L1 = [ X || X <- L, X =< H ],
    L2 = [ X || X <- L, X > H ],
    SELF = self(),
    REF = make_ref(),
    Job = fun () -> jsort(L2) end,
    scheduler ! {require, SELF, REF, Job},                      % L'atomo scheduler, grazie al costrutto register(), punta al PID dell'attore scheduler.
    SL1 = jsort(L1),
    SL2 = receive {REF, RES} -> RES end,
    SL1 ++ [ H | SL2 ].


% Funzione che permette di calcolare il tempo di esecuzione impiegato dalla funzione.
benchmark(F, L) ->
    T = [ timer:tc(?MODULE, F, L) || _ <- lists:seq(1, 10) ],
    lists:sum([ X || {X, _} <- T ]) / (1000 * length(T)).


% Main dell'attore. Viene generata una lista di numeri random utilizzata 
% per testare i diversi approcci implementati. Vengono quindi stampati i tempi 
% (in ms) di esecuzione per ogni funzione, andando a forzare una garbage collection 
% dopo ogni esecuzione, per evitare possibili influenze sul tempo di esecuzione.
main() -> 
    L = [ rand:uniform(10000) || _ <- lists:seq(1, 10000) ],
    io:format("Sequenziale: ~p~n", [ benchmark(qsort, [L]) ]),
    erlang:garbage_collect(),

    io:format("Psort: ~p~n", [ benchmark(psort, [L]) ]),
    erlang:garbage_collect(),

    io:format("Psort2(0): ~p~n", [ benchmark(psort2, [0, L]) ]),
    erlang:garbage_collect(),
    io:format("Psort2(1): ~p~n", [ benchmark(psort2, [1, L]) ]),
    erlang:garbage_collect(),
    io:format("Psort2(2): ~p~n", [ benchmark(psort2, [2, L]) ]),
    erlang:garbage_collect(),
    io:format("Psort2(3): ~p~n", [ benchmark(psort2, [3, L]) ]),
    erlang:garbage_collect(),
    io:format("Psort2(5): ~p~n", [ benchmark(psort2, [5, L]) ]),
    erlang:garbage_collect(),
    io:format("Psort2(8): ~p~n", [ benchmark(psort2, [8, L]) ]),
    erlang:garbage_collect(),
    io:format("Psort2(12): ~p~n", [ benchmark(psort2, [12, L]) ]),
    erlang:garbage_collect(),

    % Viene utilizzato il costrutto register() per registrare un nome (atomo) rispetto al PID di un attore.
    % Questo permette di avere localmente una sorta di DNS per l'attore. Alla fine dell'esecuzione questa 
    % registrazione viene annullata tramite il costrutto unregister(). Vengono inoltre spawnati 24 worker.
    %
    % Nota: se si prova a registrare due volte lo stesso attore viene sollevata una exception.
    SCHED = spawn(?MODULE, scheduler, [[]]),
    register(scheduler, SCHED),                                 
    [ spawn(?MODULE, worker, []) || _ <- lists:seq(1, 24) ],
    io:format("Jsort: ~p~n", [ benchmark(jsort, [L]) ]),
    unregister(scheduler),
    erlang:garbage_collect().


% CONSIDERAZIONI FINALI:
% I risultati ci permettono di concludere che l'implementazione base di psort sia la versione più lenta, anche rispetto all'implementazione sequenziale. 
% Questo è dovuto a due cause principali:
%       - Overhead varii, causati ad esempio da liste troppo piccole
%       - Troppi attori rispetto ai core della propria CPU, causando un numero di context-switch elevato
% 
% Bisogna quindi gestire al meglio la parallelizzazione, altrimenti si ottengono risultati peggiori rispetto all'approccio sequenziale.
%
% La seconda versione di psort, che tiene in considerazione il numero di core del processore per gestire al meglio la parallelizzazione, ottiene risultati migliori
% rispetto al psort base. Va però usato il valore giusto di N, altrimenti i tempi vanno man mano a salire fino ad arrivare ai risultati del psort base.
% Nel nostro esempio, vengono osservati i risultati con valori di N differenti, notando che con N = 8 si ottengono i risultati migliori mentre con N = 12 si peggiora
% il tempo di esecuzione precedentemente ottenuto con N = 8. Con N = 0, i risultati ottenuti sono simili a quelli dell'implementazione sequenziale 
% (il costo dovrebbe essere uguale a quello sequenziale, ma a causa di overhead è leggermente superiore).
%
% La terza versione, con pool di worker e scheduler, non è funzionante nella sua implementazione vista in precedenza. Maggiori dettagli sul perchè sono scritti
% nei commenti della funzione stessa. Teoricamente il tempo di esecuzione di questa versione dovrebbe essere simile a quello della psort con ottimizzazione N.