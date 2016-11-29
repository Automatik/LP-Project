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
%%	DEVE ACCETTARE SIA IN FORMA poly(Monomials) CHE IN FORMA DI
%ESPRESSIONE

%In caso siano espressioni
polyplus(Poly1, Poly2, Result) :-
	as_polynomial(Poly1, P1),
	as_polynomial(Poly2, P2),
	polyplus(P1, P2, Result).
polyplus(Poly1, Poly2, poly(Result)) :-
	monomials(Poly1, Ms1),
	monomials(Poly2, Ms2),
	append(Ms1, Ms2, Ms),
	dividi(Ms, SML),
	sumLists(SML, poly(Result)). %magari riordinare i monomi prima di ritornarli

%Ms lista di monomi ordinati
dividi(Ms, SimilarMonomialsList) :-
	is_list(Ms),
	accDividi(Ms, [], SimilarMonomialsList).
accDividi([], [], []).
accDividi([], L, L) :- is_list(L).
% Caso in cui M(il monomio) non � membro di una lista dei monomi simili,
% viene aggiunto
accDividi([M| Ms], Xs, L) :-
	not(presente(M, Xs, _)),
	append(Xs, [[M]], Ys),
	accDividi(Ms, Ys, L).
%Caso in cui M � gi� membro della lista, viene aggiunto
accDividi([M| Ms], Xs, L) :-
	presente(M, Xs, SubList),
	append(SubList, [M], NewSubList),
	delete(Xs, SubList, Ys),
	append(Ys, [NewSubList], Zs),
	accDividi(Ms, Zs, L).

% presente(monomio, lista monomi simili, in quale sublist se � presente)
presente(_, [], []) :- fail.
presente(m(_, TD1, VP1), [SL| _], SL) :-
	SL = [m(_, TD2, VP2)| _],
	TD1=TD2,
	VP1=VP2.
presente(M, [_| SLs], WhichSL) :-
	is_list(SLs),
	presente(M, SLs, WhichSL).

%Somma le liste di monomi simili interne alla lista
sumLists(SimilarMonomialsList, SummedMonomialsList) :-
	is_list(SimilarMonomialsList),
	accSumLists(SimilarMonomialsList, [], SummedMonomialsList).
accSumLists([], [], []).
accSumLists([], L, L) :- is_list(L).
accSumLists([SubList| SLs], Xs, SML) :-
	sumMonomials(SubList, SummedSL),
	append(Xs, [SummedSL], Ys),
	accSumLists(SLs, Ys, SML).

sumMonomials([], m(0, _, _)).
sumMonomials([m(C1, TD, VP)| Ms], MonomialResult) :-
	sumMonomials(Ms, m(C2, TD, VP)),
	C3 is C1+C2,
	MonomialResult = m(C3, TD, VP).

% polyplus(poly([m(C1, TD, VP1)| MS1]), poly([m(C2, TD, VP2)| MS2]), poly([m(C3, TD, VP3)| MS3])) :-
%	C3 is C1+C2

/*polyplus(X, Y) :-
	is_list(X),
	monomials(X, X1),
	polyplus_sorted(X1 ,Y).

polyplus_sorted([], []).
polyplus_sorted([m(C1, T, [V | Vs])], [m(C1, T, [V | Vs])]).
polyplus_sorted([m(C1, T, [V | Vs]), m(C2, T, [V | Vs]) | Xs], [m(C3, T, [V | Vs]) | Ys]) :-
	C3 is C1+C2,
	polyplus_sorted([m(C3, T, [V | Vs]) | Xs], [m(C3, T, [V | Vs]) | Ys]).
polyplus_sorted([m(C1, T, [V | Vs]), m(C2, T1, [V1 | Vs1]) | Xs], [m(C1, T, [V | Vs]) | Ys]) :-
	(Vs\=Vs1;V\=V1),
	polyplus_sorted([m(C2, T1, [V1 | Vs1]) | Xs], Ys). */

%%% polyminus(Poly1, Poly2, Result)

/*polyminus(X, Y) :-
        is_list(X),
	monomials(X, X1),
	polyminus_sorted(X1 ,Y).

polyminus_sorted([], []).
polyminus_sorted([m(C1, T, [V | Vs])], [m(C1, T, [V | Vs])]).
polyminus_sorted([m(C1, T, [V | Vs]), m(C2, T, [V | Vs]) | Xs], [m(C3, T, [V | Vs]) | Ys]) :-
	C3 is C1-C2,
	polyminus_sorted([m(C3, T, [V | Vs]) | Xs], [m(C3, T, [V | Vs]) | Ys]).
polyminus_sorted([m(C1, T, [V | Vs]), m(C2, T1, [V1 | Vs1]) | Xs], [m(C1, T, [V | Vs]) | Ys]) :-
	(Vs\=Vs1;V\=V1),
	polyminus_sorted([m(C2, T1, [V1 | Vs1]) | Xs], Ys). */


as_monomial(E, m(E, 0, [])) :-
	number(E).
/*as_monomial(E, m(E, 0, [])) :-
	compound(E),
	functor(E, Fun, N),
	atom(Fun),
	arg(N, E, V),
	number(V).*/
as_monomial(E, m(1, 1, [v(1, E)])) :-
	atom(E).
as_monomial(E, m(C, 1, [v(1, V)])) :-
	E = C*V,
	number(C),
	atom(V).
%Per esponente negativo scrivere x^(-2)
as_monomial(E, m(1, Exp, [v(Exp, B)])) :-
	E = B^Exp.
as_monomial(E, m(C, Exp, VPs)) :-
	E = V1*V2,
	as_monomial(V2, m(1, Exp1, VP1)),
	as_monomial(V1, m(C, Exp2, VP2)),
	Exp is Exp1+Exp2,
        append(VP1, VP2, VPs).
as_monomial(E, m(C, Exp, VPs)) :-
	E = V1/V2,
	as_monomial(V2, m(1, Exp1, VP1)),
	as_monomial(V1, m(C, Exp2, VP2)),
	Exp is Exp2-Exp1,
	append(VP1, VP2, VPs).
/*as_monomial(E, m(C, Exp, [v(-Exp1, V)| T])) :-
	E = V1/V2,
	as_monomial(V2, m(1, Exp1, v(Exp1, V))),
	as_monomial(V1, m(C, Exp2, T)),
	Exp is Exp2-Exp1.*/



%Solo monomio
as_polynomial(E, poly([M])) :-
	as_monomial(E, M).
as_polynomial(E, poly(Monomials)) :-
	E = M1+M2,
	as_polynomial(M2, poly(M)),
	as_polynomial(M1, poly(Ms)),
        append(M, Ms, Monomials).
as_polynomial(E, poly(Monomials)) :-
	E = M1-M2,
	M3 is -M2,
	as_polynomial(M3, poly(M)),
	as_polynomial(M1, poly(Ms)),
	append(M, Ms, Monomials).

reverse(List, RevL) :- accRev(List, [], RevL).
accRev([], Acc, Acc).
accRev([H| T], Acc, RevL) :- accRev(T, [H|Acc], RevL).



/*as_monomial(S, X) :-
	atom_codes(S, X1),
	parse_monomial(X1, X),
	list_power([X], Y),
	sum_power(Y, Y1),
	add_power(Y1, X).

add_power([Y1], m(_, Y1, _)).

parse_monomial([], m(_, _, [])).

parse_monomial([X | Xs], m(X1, T, [V | Vs])) :-
	X > 48,
	X < 58,
	number_chars(X1, [X]),
	parse_monomial(Xs, m(X1, T, [V | Vs])).

parse_monomial([X | Xs], m(X2, T, [V | Vs])) :-
	X > 48,
	X < 58,
	number_chars(X1, [X]),
	K is (X2*10)+X1,
	parse_monomial(Xs, m(K, T, [V | Vs])).

parse_monomial([X | Xs], m(C, T, [V | Vs])) :-
	X = 42,
	parse_monomial(Xs, m(C, T, [V | Vs])).

parse_monomial([X, Y, Z | Xs], m(C, T, [v(Z1, X1) | Vs])) :-
	X > 96,
	X < 123,
	Y = 94,
	Z > 48,
	Z < 58,
	atom_codes(X1, [X]),
	number_chars(Z1, [Z]),
	parse_monomial(Xs, m(C, T, Vs)).

parse_monomial([X | Xs], m(C, T, [v(1, X1) | Vs])) :-
	X > 96,
	X < 123,
	atom_codes(X1, [X]),
	parse_monomial(Xs, m(C, T, Vs)). */



%%% end of file -- polynomials_predicates.pl









