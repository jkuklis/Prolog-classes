/**
        Jakub Kuklis jk371125
*/

% przestaw(L, W)
% L - posortowana lista liczb
% W - lista liczb z L posortowana po wartosci bezwzglednej

przestaw([], []).

przestaw([X|L], W) :- rozdziel([X|L], U, D), odwrotna(U, RU), sklej(RU, D, W).


% sklej(U, D, P)
% U - lista ujemnych liczb posortowana po w.b.
% D - lista dodatnich liczb posortowana po w.b.
% P - lista liczb z U i D posortowana po w.b.

sklej([], D, D).

sklej(U, [], U).

sklej([X|U], [Y|D], [X|W]) :- X + Y >= 0, sklej(U, [Y|D], W).

sklej([Y|U], [X|D], [X|W]) :- X + Y =< 0, sklej([Y|U], D, W).


% rozdziel(L, U, D)
% L - posortowana lista liczb
% U - lista ujemnych liczb z L posortowana po w.b.
% D - lista dodatnich liczb z L posortowana po w.b.

rozdziel([], [], []).

rozdziel([X|D], [], [X|D]) :- X >= 0.

rozdziel([X|L], [X|U], D) :- X < 0, rozdziel(L, U, D).


% odwrotna(L, R)
% L - lista
% R - lista odwrotna do L

odwrotna(L, R) :- odwrotna(L, [], R).

odwrotna([], R, R).

odwrotna([X|L], A, R) :- odwrotna(L, [X|A], R).

