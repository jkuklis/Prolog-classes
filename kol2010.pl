norm(E, W) :- write(E), nl, zbierz(E, A, S).

zbierz(E, E, []) :- integer(E), !.

zbierz(E, 0, [E]).

zbierz(E1+E2, A, S) :- write(E1), nl, write(E2), nl, zbierz(E1, A1, S1), zbierz(E2, A2, S2), A is A1 + A2, append(S1, S2, S).

