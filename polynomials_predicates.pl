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
variables([m(_,_, [v(Power, VarSymbol) | Vs])| Xs], [VarSymbol| Ys] ) :-
	is_varpower(v(Power, VarSymbol)),
	variables([m(_, _, Vs) | Xs], Ys).
variables([m(_,_, []) | Xs], Ys ) :-
	variables(Xs , Ys).
variables(poly(Ms), VarsOrdered) :-
	variables(Ms, Vars),
	sort(Vars, VarsOrdered).


%%%	is_monomial(m(Coefficient, TotalDegree, VarsPowers))
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).  %E quando il monomio ha solo una variabile?

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
monomials(poly(X), poly(Y)) :-
	list_power(X, X1),
	sum_power(X1, X2),
	ordina_monomi(X , M),
	mergesort(X2, _, M, X3),
	ordina_stesso_grado(X3, Y).

ordina_stesso_grado(Ms, L) :-
	mindegree(Ms, MinG),
	maxdegree(Ms, MaxG),
	confronta(Ms, MinG, MaxG, [], L).

confronta(_, G1, G, L, L) :-
	is_list(L),
	G1 is G+1.
confronta(Ms, G, MaxG, ListaProvv, List) :-
	estrai_monomi(Ms, G, Xs),
	sort(3, @=<, Xs, Ys),
	sort([3, 1, 2], @=<, Ys, Ys1),
	append(ListaProvv, Ys1, Zs),
	G1 is G+1,
	confronta(Ms, G1, MaxG, Zs, List).

estrai_monomi([], _, []).
estrai_monomi([m(_, TD, _)| Ms], G, Xs) :-
	TD \= G,
	estrai_monomi(Ms, G, Xs).
estrai_monomi([m(C, G, VP)| Ms], G, [m(C, G, VP)| Xs]) :-
	estrai_monomi(Ms, G, Xs).

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

%polyminus, fare Poly1+(-Poly2)
polyminus(Poly1, Poly2, Result) :-
	as_polynomial(Poly1, P1),
	as_polynomial(Poly2, P2),
	polyminus(P1, P2, Result).
polyminus(Poly1, Poly2, poly(Result)) :-
	monomials(Poly1, Ms1),
	monomials(Poly2, Ms2),
	negateMonomials(Ms2, NegMs2),
	append(Ms1, NegMs2, Ms),
	dividi(Ms, SML),
	sumLists(SML, poly(Result)).

polytimes(Poly1, Poly2, Result) :-
	as_polynomial(Poly1, P1),
	as_polynomial(Poly2, P2),
	polytimes(P1, P2, Result).
polytimes(Poly1, Poly2, poly(Result)) :-
	is_polynomial(Poly1),
	is_polynomial(Poly2),
	multiplicatePolynomials(Poly1, Poly2, MML),
	monomials(poly(MML), Ms),
	dividi(Ms, SML),
	sumLists(SML, poly(Result)).

multiplicatePolynomials(poly(Ms1), poly(Ms2), MML) :-
	is_list(Ms1),
	is_list(Ms2),
	accMultiplicatePolynomials(Ms1, Ms2, MML).
accMultiplicatePolynomials([], [], []).
accMultiplicatePolynomials(_, [], []).
%accMultiplicatePolynomials(_, [], L, L) :- is_list(L).
accMultiplicatePolynomials([X| Xs], [Y| Ys], [Z| Zs]) :-
	multiplicateMonomials([X| Xs], Y, Z),
	accMultiplicatePolynomials([X| Xs], Ys, Zs).

multiplicateMonomials([], _, []).
multiplicateMonomials([m(C1, TD1, VP1)| Xs], m(C2, TD2, VP2), [m(C, TD, VP)| Zs]) :-
	C is C1*C2,
	TD is TD1+TD2,
	sumVariables(VP1, VP2, VP),
	multiplicateMonomials(Xs, m(C2, TD2, VP2), Zs).

sumVariables([], [], []).
sumVariables([], VPs, VPs) :- is_list(VPs).
sumVariables(VPs, [], VPs) :- is_list(VPs).
sumVariables(v(P, V), Ys, [SingleVariable| Zs]) :-
	extractVariable(v(_, V), Ys, Vs2),
	delete([Ys], v(_, V), Zs),
	append([v(P, V)], Vs2, Vs),
	sumPowers(Vs, SingleVariable).
sumVariables(Xs, v(P, V), [SingleVariable| Zs]) :-
	extractVariable(v(_, V), Xs, Vs1),
	delete([Xs], v(_, V), Zs),
	append([v(P, V)], Vs1, Vs),
	sumPowers(Vs, SingleVariable).

sumVariables([v(P, V)| Xs], Ys, [SingleVariable| Zs]) :-
	extractVariable(v(_, V), [v(P, V)| Xs], Vs1),
	extractVariable(v(_, V), Ys, Vs2),
	delete(Xs, v(_, V), Xs2),
	delete(Ys, v(_, V), Ys2),
	append(Vs1, Vs2, Vs),
	sumPowers(Vs, SingleVariable),
	sumVariables(Xs2, Ys2, Zs).

extractVariable(v(_, _), [], []).
extractVariable(v(_, V), v(P, V), [v(P, V)]).
extractVariable(v(_, V), v(_, X), []) :- X \= V.
extractVariable(v(_, V), [v(P, V)| Xs], [v(P, V)| Vs]) :-
	extractVariable(v(_, V), Xs, Vs).
extractVariable(v(_, V), [v(_, X)| Xs], Vs) :-
	X \= V,
	extractVariable(v(_, V), Xs, Vs).

sumPowers([], v(0, _)).
sumPowers([v(P1, V)| Vs], v(P, V)) :-
	sumPowers(Vs, v(P2, V)),
	P is P1+P2.


%Ms lista di monomi ordinati
dividi(Ms, SimilarMonomialsList) :-
	is_list(Ms),
	accDividi(Ms, [], SimilarMonomialsList).
accDividi([], [], []).
accDividi([], L, L) :- is_list(L).
% Caso in cui M(il monomio) non è membro di una lista dei monomi simili,
% viene aggiunto
accDividi([M| Ms], Xs, L) :-
	not(presente(M, Xs, _)),
	append(Xs, [[M]], Ys),
	accDividi(Ms, Ys, L).
%Caso in cui M é già membro della lista, viene aggiunto
accDividi([M| Ms], Xs, L) :-
	presente(M, Xs, SubList),
	append(SubList, [M], NewSubList),
	delete(Xs, SubList, Ys),
	append(Ys, [NewSubList], Zs),
	accDividi(Ms, Zs, L).

% presente(monomio, lista monomi simili, in quale sublist se è presente)
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

%Data una lista di monomi, li nega tutti
negateMonomials(Monomials, NegMonomials) :-
	is_list(Monomials),
	accNegateMonomials(Monomials, [], NegMonomials).
accNegateMonomials([], [], []).
accNegateMonomials([], L, L) :- is_list(L).
accNegateMonomials([m(C, TD, VP)| Ms], Xs, NegMs) :-
	C1 is -C,
	append(Xs, [m(C1, TD, VP)], Ys),
	accNegateMonomials(Ms, Ys, NegMs).

%Metodo più corto, manca caso base
/*negateMonomials([m(C, TD, VP)| Xs], [m(-C, TD, VP)| Ys]) :-
	negateMonomials(Xs, Ys). */

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

is_number_list([]).
is_number_list([N| Ns]) :-
	arithmetic_expression_value(N, Value),
	number(Value),
	is_number_list(Ns).

indexOf([E| _], E, 0) :- !.
indexOf([_| T], E, Index) :-
	indexOf(T, E, Index1),
	!,
	Index is Index1+1.

polyval(Poly, VariableValues, Value) :-
	as_polynomial(Poly, P),
	polyval(P, VariableValues, Value).
polyval(poly(Ms), VariableValues, Value) :-
	variables(poly(Ms), Vars),
	is_number_list(VariableValues),
	sostituisci(Ms, Vars, VariableValues, Xs),
	calcola(Xs, Value).

sostituisci([], _, _, []).
sostituisci([m(C, TD, VP)| Ms], Vars, VariableValues, [m(C, TD, VPs)| Xs]) :-
	cambia(VP, Vars, VariableValues, VPs),
	sostituisci(Ms, Vars, VariableValues, Xs).

cambia([], _, _, []).
cambia([v(C, V)| Vs], Vars, VariableValues, [v(C, Value)| Xs]) :-
	indexOf(Vars, V, Index),
	nth0(Index, VariableValues, Value),
	cambia(Vs, Vars, VariableValues, Xs).

calcola([], 0).
calcola([m(C, _, VP)| Ms], TotalValue) :-
	calcolaVars(VP, SingleValue),
	calcola(Ms, Rest),
	TotalValue is C*SingleValue+Rest.

calcolaVars([], 1).
calcolaVars([v(E, B)| Vs], Value) :-
	pow(B, E, V),
	calcolaVars(Vs, Rest),
	Value is V*Rest.


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
as_monomial(-E, m(-1, 1, [v(1, E)])) :-
	atom(E).
as_monomial(E, m(C, 1, [v(1, V)])) :-
	(E = C*V; E = V*C),
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
as_monomial(E, m(-C, Exp, VPs)) :-
	E = -(V1*V2),
	as_monomial(V2, m(1, Exp1, VP1)),
	as_monomial(V1, m(C, Exp2, VP2)),
	Exp is Exp2+Exp1,
	append(VP1, VP2, VPs).


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
	as_polynomial(-M2, poly(M)),
	as_polynomial(M1, poly(Ms)),
	append(M, Ms, Monomials).


pprint_polynomial(poly([m(C, _, [v(N, V) | Vs]) | Ps])) :-
	C\=[],
	C\=0,
	C>0,
	C\=1,
	N\=1,
	write(+),
	write(C*V^N),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m(C, _, [v(N, V) | Vs]) | Ps])) :-
	C\=[],
	C\=0,
	C<0,
	N\=1,
	write(C*V^N),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m(C, _, [v(N, V) | Vs]) | Ps])) :-
	C\=[],
	C\=0,
	C=1,
	N\=1,
	write(+),
	write(V^N),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m(C, _, [v(1, V) | Vs]) | Ps])) :-
	C\=[],
	C\=0,
	C>0,
	C\=1,
	write(+),
	write(C*V),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m(C, _, [v(1, V) | Vs]) | Ps])) :-
	C\=[],
	C\=0,
	C<0,
	write(C*V),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m(C, _, [v(1, V) | Vs]) | Ps])) :-
	C\=[],
	C\=0,
	C=1,
	write(+),
	write(V),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m(C, _, [_ | _]) | Ps])) :-
	C=0,
	pprint_polynomial(poly(Ps)).


pprint_polynomial(poly([m([], _, [v(N, V) | Vs]) | Ps])) :-
	Vs\=[],
	N\=1,
	write(*),
	write(V^N),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m([], _, [v(N, V)]) | Ps])) :-
	N\=1,
	write(*),
	write(V^N),
	pprint_polynomial(poly(Ps)).

pprint_polynomial(poly([m([], _, [v(1, V) | Vs]) | Ps])) :-
	Vs\=[],
	write(*),
	write(V),
	pprint_polynomial(poly([m([], _, Vs) | Ps])).

pprint_polynomial(poly([m([], _, [v(1, V)]) | Ps])) :-
	write(*),
	write(V),
	pprint_polynomial(poly(Ps)).

pprint_polynomial(poly([m([], _, []) | Ps])) :-
		pprint_polynomial(poly(Ps)).

pprint_polynomial(poly([])) :-
	fail.



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









