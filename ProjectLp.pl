% This buffer is for notes you don't want to save.
% If you want to create a file, visit that file with C-x C-f,
% then enter the text in that file's own buffer.

is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

is_varpower(v(Power, VarSymbol)) :-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

coefficients([], []).
coefficients(m(Coefficient, _, _), Coefficient).
coefficients([m(Coefficient, _, _)| Xs], [Coefficient| Ys]) :-
	coefficients(Xs, Ys).

variables([], []).
variables(m(_, _, v(_, VarSymbol)), VarSymbol).
variables([m(_, _, v(_, VarSymbol))| Xs], [VarSymbol| Ys]) :-
	variables(Xs, Ys).

