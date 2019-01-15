norm(E, W) :- nl, zbierz(E, A, S), przypisz(A, S, W).

zbierz(E, E, []) :- integer(E), !.

zbierz(E1+E2, A, S) :- !, zbierz(E1, A1, S1), zbierz(E2, A2, S2), A is A1 + A2, append(S1, S2, S).

zbierz(E, 0, [E]).

przypisz(A, [], A) :- !.

przypisz(0, S, N) :- !, forma(S, N).

przypisz(A, [X], A+X) :- !.

przypisz(A, S, A+(S1)) :- forma(S, S1).

forma([X, Y], X+Y).

forma([X | S], X+(S1)) :- forma(S, S1).
