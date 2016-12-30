%%% -*- Mode: Prolog -*-

test_coefficients() :-
	coefficients(y^0, [1]),
	coefficients(0*x-3, [-3]), %coefficiente nullo, si può sorvolare
	coefficients(-y*x+y, [-1, 1]),
	coefficients(0*x, []), %coeff nullo
	coefficients(poly([m(-3, 0, []), m(-4, 1, [v(1, x)])]), [-3, -4]),
	coefficients(m(-1, 1, [v(1, x)]), [-1]),
	coefficients(m(1, 0, []), [1]).

test_variables() :-
	variables(x*y*z, [x, y, z]),
	variables(0*x*y, []), %coefficiente 0
	variables(x^0, []), %grado 0
	variables(x+y^2, [x, y]),
	variables(3*b-a, [a, b]),
	variables(m(1, 0, []), []),
	variables(poly([m(-3, 0, []), m(-4, 1, [v(1, x)])]), [x]).

test_monomials() :-
	monomials(x*y+3, [m(3, 0, []), m(1, 2, [v(1, x), v(1, y)])]),
	monomials(0*x-y, [m(-1, 1, [v(1, y)])]),
	monomials(-pippo^123*pluto, [m(-1, 124, [v(123, pippo), v(1, pluto)])]),
	monomials(x+x+x+x, [m(4, 1, [v(1, x)])]),
	monomials(x+x+x+0*x, [m(3, 1, [v(1, x)])]).

test_maxdegree() :-
	maxdegree(x^0, 0),
	maxdegree(pippo^3*pippo, 4),
	maxdegree(a^3-b+ciao^2, 3),
	maxdegree(-a-b^7-c^2, 7),
	maxdegree(-b-b^7-b^2, 7).

test_mindegree() :-
	mindegree(x^0, 0),
	mindegree(pippo^3*pippo, 4),
	mindegree(a^3-b+ciao^2, 1),
	mindegree(-a-b^7-c^2, 1),
	mindegree(-b^0-b^7-b^2, 0). %grado 0, ma coeff è sempre 1 (o -1 in questo caso)

test_polyplus() :-
	polyplus(a^3, 3*b, poly([m(3, 1, [v(1, b)]), m(1, 3, [v(3, a)])])),
	polyplus(0*x, b, poly([m(1, 1, [v(1, b)])])),
	polyplus(a+c^3, a^0, poly([m(1, 0, []), m(1, 1, [v(1, a)]), m(1, 3, [v(3, c)])])),
	polyplus(x^0, 3*x^0, poly([m(4, 0, [])])),
	polyplus(3, 5, poly([m(8, 0, [])])),
	polyplus(m(-1, 1, [v(1, x)]), 3 * x, poly([m(2, 1, [v(1, x)])])),
	polyplus(poly([m(1, 2, [v(1, a), v(1, b)])]), m(1, 2, [v(1, a), v(1, b)]), poly([m(2, 2, [v(1, a), v(1, b)])])).

test_polyminus() :-
	polyminus(a^3, 3*b, poly([m(-3, 1, [v(1, b)]), m(1, 3, [v(3, a)])])),
	polyminus(0*x, b, poly([m(-1, 1, [v(1, b)])])),
	polyminus(a + c ^ 3, a ^ 0, poly([m(-1, 0, []), m(1, 1, [v(1, a)]), m(1, 3, [v(3, c)])])),
	polyminus(x ^ 0, 3 *x ^ 0, poly([m(-2, 0, [])])),
	polyminus(3, 5, poly([m(-2, 0, [])])),
	polyminus(m(-1, 1, [v(1, x)]), 3 * x, poly([m(-4, 1, [v(1, x)])])),
	polyminus(poly([m(1, 2, [v(1, a), v(1, b)])]), m(1, 2, [v(1, a), v(1, b)]), poly([])). %coeff è zero, elimino monomio

test_polytimes() :-
	polytimes(1, a * b * c, poly([m(1, 3, [v(1, a), v(1, b), v(1, c)])])),
	polytimes(0, a * b * c, poly([])),
	polytimes(a^0, a, poly([m(1, 1, [v(1, a)])])),
	polytimes(pippo * pluto, 0 + pippo, poly([m(1, 3, [v(2, pippo), v(1, pluto)])])),
	polytimes(x + y, a * b * c, poly([m(1, 4, [v(1, a), v(1, b), v(1, c), v(1, x)]), m(1, 4, [v(1, a), v(1, b), v(1, c), v(1, y)])])),
	polytimes(x + y, a + b, poly([m(1, 2, [v(1, a), v(1, x)]), m(1, 2, [v(1, a), v(1, y)]), m(1, 2, [v(1, b), v(1, x)]), m(1, 2, [v(1, b), v(1, y)])])),
	polytimes(a + b, a + b, poly([m(2, 2, [v(1, a), v(1, b)]), m(1, 2, [v(2, a)]), m(1, 2, [v(2, b)])])).

test_as_monomial() :-
	as_monomial(3 * ciao ^2 * bea ^ 3, m(3, 5, [v(3, bea), v(2, ciao)])),
	as_monomial(3 * x ^ 0,m(3, 0, [])),
	as_monomial(0 * a * b * c * quellochevoglio, m(0, 0, [])),
	as_monomial(-foo^42, m(-1, 42, [v(42, foo)])),
	as_monomial(-42 * foo * bar ^ 42, m(-42, 43, [v(42, bar), v(1, foo)])),
	as_monomial(pippo * pippo * pippo * pippo * pippo^5, m(1, 9, [v(9, pippo)])),
	as_monomial(a * b * a * b * a * c * b, m(1, 7, [v(3, a), v(3, b), v(1, c)])).

test_as_polynomial() :-
	as_polynomial( 0 + 0 + 0, poly([])),
	as_polynomial( 0 + 0 + x, poly([m(1, 1, [v(1, x)])])),
	as_polynomial(a^2 + b^2 + a * b + b * a, poly([m(2, 2, [v(1, a), v(1, b)]), m(1, 2, [v(2, a)]), m(1, 2, [v(2, b)])])),
	as_polynomial(pippo + gennaro + 3 * pippo, poly([m(1, 1, [v(1, gennaro)]), m(4, 1, [v(1, pippo)])])),
	as_polynomial(a * b + c * d + a * d + s * d, poly([m(1, 2, [v(1, a), v(1, b)]), m(1, 2, [v(1, a), v(1, d)]), m(1, 2, [v(1, c), v(1, d)]), m(1, 2, [v(1, d), v(1, s)])])),
	as_polynomial(-3 * pippo -4*gennaro + 1, poly([m(1, 0, []), m(-4, 1, [v(1, gennaro)]), m(-3, 1, [v(1, pippo)])])),
	as_polynomial(-3 * a ^ 4, +12 * a ^2 * a ^2, poly([m(9, 4, [v(4, a)])])).

test_polyval() :-
	polyval(3, [], 3),
	polyval(-x, [12], -12),
	polyval(-2*x + y,[3, 2], -4),
%	polyval(0 * x * pluto, [2, 1], PV4),
	polyval(x * pippo, [0, 100000], 0),
	polyval(x ^ 0, [], 1),
	polyval(poly([m(1, 1, [v(1, a)]), m(4, 1, [v(1, x)])]), [1, 1], 5),
	polyval(poly([m(3, 1, [v(1, x)]), m(1, 2, [v(2, a)])]), [-1, -2], -5).
