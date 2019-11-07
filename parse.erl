-module(parse).

-export([tokenize/1]).

-record(state,
	{queue = "", queue_number = "", minus_unary = true}).

tokenize(S) -> tokenize(S, #state{}).

tokenize([], State)
    when State#state.queue_number =/= [] ->
    State#state.queue ++ [State#state.queue_number];
tokenize([], State) -> State#state.queue;
tokenize([H | T], State) ->
    S = case H of
	  $( -> addToQueue(H, setMinus(State, true));
	  $) -> addToQueue(H, setMinus(State, false));
	  $+ -> addToQueue(H, setMinus(State, true));
	  $- when State#state.minus_unary =:= false ->
	      addToQueue(H, setMinus(State, true));
	  $- ->
	      State#state{queue_number =
			      State#state.queue_number ++ [$-],
			  minus_unary = false};
	  $/ -> addToQueue(H, setMinus(State, true));
	  $* -> addToQueue(H, setMinus(State, true));
	  Char
	      when Char >= $0, Char =< $9; Char =:= $.; Char =:= $, ->
	      State#state{queue_number =
			      State#state.queue_number ++ [Char],
			  minus_unary = false};
	  32 when State#state.queue_number =/= [] ->
	      State#state{queue =
			      State#state.queue ++ [State#state.queue_number],
			  queue_number = ""};
	  _ -> State
	end,
    tokenize(T, S).

addToQueue(V, State) ->
    case State#state.queue_number of
      [] -> State#state{queue = State#state.queue ++ [[V]]};
      X ->
	  State#state{queue = State#state.queue ++ [X] ++ [[V]],
		      queue_number = ""}
    end.

setMinus(State, Minus_unary) ->
    State#state{minus_unary = Minus_unary}.
