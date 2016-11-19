%%% -*- Mode: Prolog -*-

max(List, Max) :-
	List = [H| _],
	accMax(List, H, Max).
accMax([], Max, Max).
accMax([H| T], Acc, Max) :-
	H > Acc,
	accMax(T, H, Max).
accMax([H| T], Acc, Max) :-
	H =< Acc,
	accMax(T, Acc, Max).

min(List, Min) :-
	List = [H| _],
	accMin(List, H, Min).
accMin([], Min, Min).
accMin([H| T], Acc, Min) :-
	H < Acc,
	accMin(T, H, Min).
accMin([H| T], Acc, Min) :-
	H >= Acc,
	accMin(T, Acc, Min).


%%% end of file -- utils.pl
