-module(div_server).

-export([start_link/0]).

start_link() ->
    Pid = spawn_link(fun init/0),
    global:register_name("divide", Pid).

init() -> loop().

loop() ->
    receive
      {From, Ref, {divide, Param1, Param2}} ->
	  io:format("Div: ~p / ~p~n", [Param1, Param2]),
	  if Param2 =:= 0 -> From ! {error, Ref, divide_by_zero};
	     true -> From ! {result, Ref, Param1 / Param2}
	  end;
      {_, ref, terminate} -> terminate();
      Unknown ->
	  io:format("Unknown message: ~p~n", [Unknown]), loop()
    end,
    loop().

terminate() -> ok.
