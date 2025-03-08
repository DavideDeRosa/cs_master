-module(dict).
-export([search/2, insert/3, search_val/2, search_val_aux/2, search_val2/2]).

% Alberi binari di ricerca con chiavi nei nodi interni e coppie 
% chiavi-payload nelle foglie
%
% Esempio Algebraic Data Type: tipo di dato che ha un certo numero 
% finito di forme distinte dove ogni forma è un tipo di dato composto.
%
% Un albero binario di ricerca è un esempio di dato algebrico, definito come:
%       Tree K V ::= { leaf, K, V } | { node, Tree K V, K, Tree K V }
%
% Il | indica più possibili forme. leaf e node indicano vengono aggiunti per 
% discriminare le forme possibili. Nel nostro caso non sarebbe stato necessario, 
% ma in casi dove la lunghezza delle tuple è uguale è un buon modo per 
% differenziare più forme. Questo permette di avere codice più robusto.
%
% Esempio:
%      { node, { leaf, 2, anna }, 4, { leaf, 5 , bruno} }
%  
%                             4
%                           /   \
%                { 2, anna }     { 5, bruno }
%
%
% Tipo option, il quale permette di identificare se un valore è stato trovato 
% o meno all'interno dell'albero. Viene definito come:
%       Option V ::= not_found | { found, V }
%
%
% Cerco un valore associato a una chiave in un albero binario di ricerca.
% Implementazione della ricerca:

search(K, {leaf, K2, V}) when K =:= K2 -> { found, V } ;
search(_, {leaf, _, _}) -> not_found ;

search(K, {node, T1, K2, _}) when K =< K2 -> search(K, T1) ;
search(K, {node, _, _, T2}) -> search(K, T2).

% Un possibile esempio di esecuzione può essere il seguente:
% Possiamo richiamare 
%
% dict:search(2, { node, { leaf, 2, anna }, 4, { leaf, 5 , bruno} })., 
% con esito {found,anna}
% 
% dict:search(5, { node, { leaf, 2, anna }, 4, { leaf, 5 , bruno} })., 
% con esito {found,bruno}
% 
% dict:search(7, { node, { leaf, 2, anna }, 4, { leaf, 5 , bruno} })., 
% con esito not_found
%
% La search è tail ricorsiva. Il suo costo è:
% - in tempo O(h), dove h è l'altezza dell'albero
% - in spazio O(1)
%
% Restituisco in output una copia dell'input dove ho cambiato l'associazione
% chiave-valore oppure ho inserito una chiave.
% Implementazione dell'inserimento:

insert(K, V, {leaf, K2, _}) when K =:= K2 -> {leaf, K, V} ;

insert(K, V, {_, K2, _} = T2) when K =< K2 -> {node, {leaf, K, V}, K, T2} ;
insert(K, V, {_, K2, _} = T2) -> {node, T2, K2, {leaf, K, V}} ;

insert(K, V, {node, T1, K2, T2}) when K =< K2 -> {node, insert(K, V, T1), K2, T2} ;
insert(K, V, {node, T1, K2, T2}) -> {node, T1, K2, insert(K, V, T2)}.

% Un possibile esempio di esecuzione può essere il seguente:
% Possiamo richiamare 
% 
% T1 = {leaf, 7, ciao}.,
% che restituisce {leaf,7,ciao}
%
% T2 = dict:insert(2, pippo, T1).,
% che restituisce {node,{leaf,2,pippo},2,{leaf,7,ciao}}
%
% T3 = dict:insert(1, pluto, T2).,
% che restituisce {node,{node,{leaf,1,pluto},1,{leaf,2,pippo}},2,{leaf,7,ciao}}
%
% T4 = dict:insert(7, pap, T3)., 
% che restituisce {node,{node,{leaf,1,pluto},1,{leaf,2,pippo}},2,{leaf,7,pap}},
% modificando il valore del nodo con valore 7 (da (7, ciao) a (7, pap)).
%
% La insert NON è tail ricorsiva. Il suo costo è:
% - in tempo O(h), dove h è l'altezza dell'albero
% - in spazio O(h), dove h è l'altezza dell'albero (dovuto interamente allo stack)
%
%
% search_val(V, T) restituisce { found, K } se K è associata a V in T, not_found altrimenti

search_val(V1, {leaf, K, V2}) when V1 =:= V2 -> {found, K} ;
search_val(_, {leaf, _, _}) -> not_found ; 

search_val(V, {node, T1, _, T2}) -> 
    case search_val(V, T1) of
        {found, _} = Res -> Res ;
        not_found -> search_val(V, T2)
    end.

% search_val_aux(V, T) solleva { found, K } se K è associata a V in T, not_found altrimenti

search_val_aux(V1, {leaf, K, V2}) when V1 =:= V2 -> throw({found, K}) ;
search_val_aux(_, {leaf, _, _}) -> not_found ; 

search_val_aux(V, {node, T1, _, T2}) -> 
    search_val_aux(V, T1),
    search_val_aux(V, T2).

% search_val2(V, T) restituisce { found, K } se K è associata a V in T, not_found altrimenti
search_val2(K, T) -> 
    try
        search_val_aux(K, T)
    catch
        {found, _} = R -> R
    end.