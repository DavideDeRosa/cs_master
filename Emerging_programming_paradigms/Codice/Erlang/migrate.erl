-module(migrate).
-export([]).


migrate(NODE, F) ->
 PID = spawn(NODE, F),
 Forward =
  fun Forward() ->
   receive
    {comeback, F} -> F() ;
    Msg -> PID ! Msg, Forward()
   end
  end,
 Forward().

server(Dati) ->
 receive
   msg1 -> server(Dati + 1) ;
   {migrate, NODE} ->
     migrate(NODE, fun () -> server(Dati) end)
 end.
