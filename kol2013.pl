neighbours(a, [b,c,d]).
neighbours(b, [e]).
neighbours(c, [d]).
neighbours(d, [a]).
neighbours(e, [e]).


cykle(V, W) :- cykle(V, [], W1), write(W1), odsiew(V, W1, W).

odsiew([], _, []).

odsiew([X | V], W, [X | R]) :- member(X, W), !, odsiew(V, W, R).

odsiew([_ | V], W, R) :- odsiew(V, W, R).


cykle([], W, W).

cykle([X | V], A, W) :- dfs(X, R), write(X), write(' '), write(R), nl, append(R, A, A1), cykle(V, A1, W).


% dfs(Root, Visited, Path, Result)

dfs(X, R) :- dfs(X, [X], [X], R).

dfs(X, V, P, R) :- neighbours(X, N), wczesniej(N, P, R1), write('wczesniej: '), write(N), write(' '), write(P), write(' '), write(R1), nl, dfsy(N, V, P, R2), append(R1, R2, R).

wczesniej([], _, []).

wczesniej([X | N], P, R) :- wczesniejH(X, P, [], R1), wczesniej(N, P, R2), append(R1, R2, R).

wczesniejH(_, [], _, []).

wczesniejH(X, [X | _], R, [X | R]) :- !.

wczesniejH(X, [Y | P], A, R) :- wczesniejH(X, P, [Y | A], R).

dfsy([], _, _, []).

dfsy([X | N], V, P, R) :- member(X, V), !, dfsy(N, V, P, R).

dfsy([X | N], V, P, R) :- dfs(X, [X | V], [X | P], R1), dfsy(N, [X | V], P, R2), append(R1, R2, R).
