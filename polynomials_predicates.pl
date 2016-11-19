%%% -*- Mode: Prolog -*-

%%	coefficients(Poly, Coefficients)
coefficients([], []).
coefficients(m(Coefficient, _, _), Coefficient) :-
	number(Coefficient).
coefficients([m(Coefficient, _, _)| Xs], [Coefficient| Ys]) :-
	number(Coefficient),
	coefficients(Xs, Ys).

%%	variables(Poly, Variables)
variables([], []).
variables(m(_, _, v(Power, VarSymbol)), VarSymbol) :-
	is_varpower(v(Power, VarSymbol)).
variables([m(_, _, v(Power, VarSymbol))| Xs], [VarSymbol| Ys]) :-
	is_varpower(v(Power, VarSymbol)),
	variables(Xs, Ys).

%%	is_monomial(m(Coefficient, TotalDegree, VarsPowers))
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

%%	is_varpower(v(Power, VarSymbol))
is_varpower(v(Power, VarSymbol)) :-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

%%	maxdegree and mindegree
maxdegree(Poly, Degree) :-
	list_degrees(Poly, Ds),
	max(Ds, Degree).
mindegree(Poly, Degree) :-
	list_degrees(Poly, Ds),
	min(Ds, Degree).
list_degrees([], []).
list_degrees([m(C, Degree, V)| Ms], [Degree| Ds]) :-
	is_monomial(m(C, Degree, V)),
	list_degrees(Ms, Ds).

%%	is_polynomial(poly(Monomials))
is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).




%%% end of file -- polynomials_predicates.pl
