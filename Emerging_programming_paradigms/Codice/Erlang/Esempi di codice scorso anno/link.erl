-module(link).
-export([main/0,f/0,linka/2,monitora/1]).

sleep(N) -> receive after N*1000 -> ok end.

f() ->
        process_flag(trap_exit, true),
        io:format("~p started~n",[self()]),
        sleep(5),
        io:format("~p terminated~n",[self()]).

linka(Trap,One) ->
        case Trap of
           true -> process_flag(trap_exit, true) ;
           false -> ok
        end,
        link(One),
        io:format("~p started, linking ~p~n",[self(),One]),
        sleep(5),
        receive Msg -> io:format("~p received: ~p~n",[self(),Msg]) end,
        io:format("~p terminated~n",[self()]).

monitora(One) ->
        monitor(process,One),
        io:format("~p started, monitoring ~p~n",[self(),One]),
        sleep(5),
        receive Msg -> io:format("~p received: ~p~n",[self(),Msg]) end,
        io:format("~p terminated~n",[self()]).

main() ->
        spawn(?MODULE,f,[]),
        One = spawn(?MODULE,f,[]),
        Two = spawn(?MODULE,linka,[false,One]),
        spawn(?MODULE,linka,[true,Two]),
        spawn(?MODULE,monitora,[One]),
        sleep(1),
        exit(One,kill).
