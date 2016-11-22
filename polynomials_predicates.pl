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
variables([m(_,_, [v(Power, VarSymbol) | Vs]) | Xs], [[VarSymbol | Zs] | Ys] ) :-
	is_varpower(v(Power, VarSymbol)),
	variables([m(_, _, Vs) | Xs], [Zs | Ys]).
variables([m(_,_, []) | Xs], [[] | Ys] ) :-
	variables(Xs , Ys).


%%%	is_monomial(m(Coefficient, TotalDegree, VarsPowers))
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

%%%	is_varpower(v(Power, VarSymbol))
is_varpower(v(Power, VarSymbol)) :-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

%%%	maxdegree and mindegree
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


%%%	is_polynomial(poly(Monomials))
is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).

%%%     monomials(Poly, Monomials)

list_power([], []).
list_power(m(_, _, v(Power, VarSymbol)), Power) :-
	is_varpower(v(Power, VarSymbol)).
list_power([m(_, _, v(Power, VarSymbol))| Xs], [Power| Ys]) :-
	is_varpower(v(Power, VarSymbol)),
	list_power(Xs, Ys).
list_power([m(_,_, [v(Power, VarSymbol) | Vs]) | Xs], [[Power | Zs] | Ys] ) :-
	is_varpower(v(Power, VarSymbol)),
	list_power([m(_, _, Vs) | Xs], [Zs | Ys]).
list_power([m(_,_, []) | Xs], [[] | Ys] ) :-
	list_power(Xs , Ys).

somma([], 0).
somma([X | Xs], S) :-
	somma(Xs, K),
	S is K + X.

sum_power([],[]).
sum_power([X | Xs],[Y | Ys]) :-
	is_list(X),
	somma(X, Y),
	sum_power(Xs, Ys).
sum_power([X | Xs], [X | Ys]) :-
	sum_power(Xs, Ys).

monomials([], []).
monomials(X, Y) :-
	list_power(X, X1),
	sum_power(X1, X2),
	ordina_monomi(X , M),
	mergesort(X2, _, M, Y).

mergesort([], [], [], []).
mergesort([A], [A], [A1], [A1]).
mergesort([A,B | R], S, [A1,B1 | R1], Z) :-
   split([A,B | R], L1, L2, [A1,B1 | R1], L3, L4),
   mergesort(L1, S1, L3, S3),
   mergesort(L2, S2, L4, S4),
   merge(S1, S2, S, S3, S4, Z).

split([], [], [], [], [], []).
split([A], [A], [], [A1], [A1], []).
split([A, B | R],[A | Ra],[B | Rb],[A1, B1 | R1],[A1 | Ra1],[B1 | Rb1]) :-
	split(R, Ra, Rb, R1, Ra1, Rb1).

merge(A, [], A, A1, [], A1).
merge([], B, B, [], B1, B1).
merge([A | Ra], [B | Rb], [A | M], [A1 | Ra1], [B1 | Rb1], [A1 | M1]) :-
	A @>= B,
	merge(Ra,[B|Rb],M,Ra1,[B1|Rb1],M1).
merge([A | Ra],[B | Rb],[B | M],[A1 | Ra1],[B1 | Rb1],[B1 | M1]) :-
	A @< B,
	merge([A | Ra], Rb, M, [A1 | Ra1], Rb1, M1).

mergesort2([], [], [], []).
mergesort2([A], [A], [A1], [A1]).
mergesort2([A,B | R], S, [A1,B1 | R1], Z) :-
   split2([A,B | R], L1, L2, [A1,B1 | R1], L3, L4),
   mergesort2(L1, S1, L3, S3),
   mergesort2(L2, S2, L4, S4),
   merge2(S1, S2, S, S3, S4, Z).

split2([], [], [], [], [], []).
split2([A], [A], [], [A1], [A1], []).
split2([A, B | R],[A | Ra],[B | Rb],[A1, B1 | R1],[A1 | Ra1],[B1 | Rb1]) :-
	split2(R, Ra, Rb, R1, Ra1, Rb1).

merge2(A, [], A, A1, [], A1).
merge2([], B, B, [], B1, B1).
merge2([A | Ra], [B | Rb], [A | M], [A1 | Ra1], [B1 | Rb1], [A1 | M1]) :-
	A @=< B,
	merge2(Ra,[B|Rb],M,Ra1,[B1|Rb1],M1).
merge2([A | Ra],[B | Rb],[B | M],[A1 | Ra1],[B1 | Rb1],[B1 | M1]) :-
	A @> B,
	merge2([A | Ra], Rb, M, [A1 | Ra1], Rb1, M1).

ordina_monomio(m( A, B, []), m( A, B, [])).
ordina_monomio(m( A, B, Vs), m( A, B, Zs)) :-
	list_var(m(_, _, Vs), Vs1),
	mergesort2(Vs1, _, Vs, Zs).

ordina_monomi([], []).
ordina_monomi([X | Xs], [Y | Ys]) :-
	is_monomial(X),
	ordina_monomio(X, Y),
	ordina_monomi(Xs, Ys).



list_var(m( _, _, []), []).
list_var(m( _, _, v(_, Vs)),  Vs ).
list_var(m( _, _, [v(_, Vs)]),  [Vs] ).
list_var(m( _, _, [v(_, Vs) | Zs ]),  [Vs | Ys] ) :-
	list_var(m( _, _, Zs), Ys).

%%% polyplus(Poly1, Poly2, Result)

polyplus([], []).
polyplus([m(C1, T, [V | Vs])], [m(C1, T, [V | Vs])]).
polyplus([m(C1, T, [V | Vs]), m(C2, T, [V | Vs]) | Xs], [m(C3, T, [Vs]) | Ys]) :-
	C3 is C1+C2,
	polyplus([m(C3, T, [V | Vs]) | Xs], [m(C3, T, [V | Vs]) | Ys]).
polyplus([m(C1, T, [V | Vs]), m(C2, T1, [V1 | Vs1]) | Xs], [m(C1, T, [V | Vs]) | Ys]) :-
	(Vs\=Vs1;V\=V1),
	polyplus([m(C2, T1, [V1 | Vs1]) | Xs], Ys).

%%% polyminus(Poly1, Poly2, Result)

polyminus([], []).
polyminus([m(C1, T, [V | Vs])], [m(C1, T, [V | Vs])]).
polyminus([m(C1, T, [V | Vs]), m(C2, T, [V | Vs]) | Xs], [m(C3, T, [Vs]) | Ys]) :-
	C3 is C1-C2,
	polyminus([m(C3, T, [V | Vs]) | Xs], [m(C3, T, [V | Vs]) | Ys]).
polyminus([m(C1, T, [V | Vs]), m(C2, T1, [V1 | Vs1]) | Xs], [m(C1, T, [V | Vs]) | Ys]) :-
	(Vs\=Vs1;V\=V1),
	polyminus([m(C2, T1, [V1 | Vs1]) | Xs], Ys).




%%% end of file -- polynomials_predicates.pl









