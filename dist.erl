-module(dist).

-export([start/1]).

-record(state, {queue = "", stack = ""}).

% Shunting-yard Algorithm by Dijkstra
convertRPN(S) -> convertRPN(S, #state{}).

convertRPN([], State) ->
    State#state.queue ++ State#state.stack;
convertRPN([H | T], State) ->
    S = case H of
	  "(" -> State#state{stack = ["(" | State#state.stack]};
	  %
	  ")" ->
	      {P, Q} = removeFromStack(State#state.stack, []),
	      State#state{queue = State#state.queue ++ Q, stack = P};
	  %
	  Char when Char =:= "+"; Char =:= "-" ->
	      {P, Q} = removeOpFromStack(State#state.stack, []),
	      State#state{queue = State#state.queue ++ Q,
			  stack = [Char | P]};
	  %
	  Char when Char =:= "*"; Char =:= "/" ->
	      checkStackPriority(Char, State);
	  %
	  Str -> State#state{queue = State#state.queue ++ [Str]}
	end,
    %   _ -> State
    convertRPN(T, S).

removeFromStack([], S) -> {[], S};
removeFromStack(["(" | T], S) -> {T, S};
removeFromStack([H | T], S) ->
    removeFromStack(T, S ++ [H]).

removeOpFromStack([], S) -> {[], S};
removeOpFromStack([H | T], S) when H =:= "(" ->
    {[H | T], S};
removeOpFromStack([H | T], S) ->
    removeOpFromStack(T, S ++ [H]).

checkStackPriority(Char, State)
    when State#state.stack =:= [] ->
    State#state{stack = [Char]};
checkStackPriority(Char, State) ->
    case hd(State#state.stack) of
      C when C =:= "+"; C =:= "-"; C =:= "(" ->
	  State#state{stack = [Char | State#state.stack]};
      H when Char =:= H ->
	  checkStackPriority(Char,
			     State#state{queue = State#state.queue ++ [H],
					 stack = tl(State#state.stack)});
      H when Char =/= H ->
	  State#state{queue = State#state.queue ++ [H],
		      stack = [Char | tl(State#state.stack)]}
    end.

% -------------------------------------------------------------------
read(N) ->
    case string:to_float(N) of
      {error, no_float} -> list_to_integer(N);
      {F, _} -> F
    end.

start(S) ->
    L = convertRPN(parse:tokenize(S)), loop(L, []).

loop([], [R]) -> R;
loop([H | T], Stack) ->
    % io:format("Q: ~p S: ~p~n", [[H | T], Stack]),
    Stq = case H of
	    "+" -> calc(add, Stack);
	    "-" -> calc(sub, Stack);
	    "*" -> calc(multiply, Stack);
	    "/" -> calc(divide, Stack);
	    Num -> [read(Num) | Stack]
	  end,
    loop(T, Stq).

calc(add, [Param1, Param2 | Stack]) ->
    Ref = make_ref(),
    global:send("add",
		{self(), Ref, {add, Param2, Param1}}),
    receive
      {result, Ref, R} -> [R | Stack];
      Unknown -> io:format("Unknown message: ~p~n", [Unknown])
      after 5000 -> timeout
    end;
calc(sub, [Param1, Param2 | Stack]) ->
    Ref = make_ref(),
    global:send("substract",
		{self(), Ref, {sub, Param2, Param1}}),
    receive
      {result, Ref, R} -> [R | Stack];
      Unknown -> io:format("Unknown message: ~p~n", [Unknown])
      after 5000 -> timeout
    end;
calc(multiply, [Param1, Param2 | Stack]) ->
    Ref = make_ref(),
    global:send("multiply",
		{self(), Ref, {multiply, Param2, Param1}}),
    receive
      {result, Ref, R} -> [R | Stack];
      Unknown -> io:format("Unknown message: ~p~n", [Unknown])
      after 5000 -> timeout
    end;
calc(divide, [Param1, Param2 | Stack]) ->
    Ref = make_ref(),
    global:send("divide",
		{self(), Ref, {divide, Param2, Param1}}),
    receive
      {result, Ref, R} -> [R | Stack];
      {error, Ref, divide_by_zero} ->
	  io:format("Error: divide by zero");
      Unknown -> io:format("Unknown message: ~p~n", [Unknown])
      after 5000 -> timeout
    end.
