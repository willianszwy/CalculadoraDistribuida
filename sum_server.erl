-module(sum_server).

-export([start_link/0]).

start_link() ->
    Pid = spawn_link(fun init/0),
    global:register_name("add", Pid).

init() -> loop().

loop() ->
    receive
      {From, Ref, {add, Param1, Param2}} ->
	  io:format("Sum: ~p + ~p~n", [Param1, Param2]),
	  From ! {result, Ref, Param1 + Param2};
      {_, ref, terminate} -> terminate();
      Unknown ->
	  io:format("Unknown message: ~p~n", [Unknown]), loop()
    end,
    loop().

terminate() -> ok.
