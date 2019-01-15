depend(G, X, S, L) :- vertices(G, V), write(V), paths(G, V, X, S, L).

paths(_, [], _, _, []).

paths(G, [W | V], X, S, [W | L]) :- path(G, X, W, S), !, paths(G, V, X, S, L).

paths(G, [_ | V], X, S, L) :- paths(G, V, X, S, L).

path(G, W, X, S) :- path(G, W, X, S, [W]).

path(G, W, X, plus, _) :- edge(G, W, X, plus).

path(G, W, X, plus, V) :-
	edge(G, W, C, plus),
	\+ member(C, V),
	path(G, C, X, plus, [C | V]).

path(G, W, X, minus, _) :- edge(G, W, X, minus).

path(G, W, X, minusAlready, _) :- edge(G, W, X, _).

path(G, W, X, minus, V) :-
	edge(G, W, C, minus),
	\+ member(C, V),
	path(G, C, X, minusAlready, [C | V]).

path(G, W, X, minusAlready, V) :-
	edge(G, W, C, _),
	\+ member(C, V),
	path(G, C, X, _, [C | V]).

path(G, W, X, minus, V) :-
	edge(G, W, C, plus),
	\+ member(C, V),
	path(G, C, X, minus, [C | V]).

edge(G, W, X, S) :- member(zal(W, X, S), G).

vertices(G, V) :- vertices(G, [], V).

vertices([], V, V).

vertices([zal(_, P, _) | G], A, V) :-
	member(P, A),
	!,
	vertices(G, A, V).

vertices([zal(_, P, _) | G], A, V) :-
	vertices(G, [P | A], V).

exG([zal(p, q, plus), zal(q, r, minus), zal(q, s, plus)]).
