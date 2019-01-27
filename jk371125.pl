% Jakub Kuklis jk371125


% krawedz(Od, Do, Wartosc)

krawedz(A, B, W) :- sasiedzi(A, N), member(kr(B, W), N).


% sciezka(Od, Do, ParzystoscSciezki)
% ParzystoscSciezki mowi o tym, jaka jest parzystosc sumy etykiet na sciezce

sciezka(A, B, W) :- sciezka(A, B, W, [A, B]).


% sciezka(Od, Do, ParzystoscSciezki, Odwiedzone)

sciezka(A, B, W, _) :- 
	krawedz(A, B, W).

sciezka(A, B, W, V) :-
	krawedz(A, C, W1),
	\+ member(C, V),
	sciezka(C, B, W2, [C | V]),
	W3 is W1 + W2,
	W is W3 mod 2.	


% osiagalne(WierzcholekStartowy, OsiagalneWierzcholki)

osiagalne(W, L) :- graf(G), osiagalne(W, G, L).


% osiagalne(WierzcholekStartowy, Pozostale, OsiagalneWierzcholkiZPozostalych)
% OsiagalneWierzcholkiZPozostalych - wierzcholki z listy Pozostale, 
% do ktorych istnieje sciezka z WierzcholelStartowy

osiagalne(_, [], []).

osiagalne(W, [X | G], [X | L]) :- sciezka(W, X, 1), !, osiagalne(W, G, L).

osiagalne(W, [_ | G], L) :- osiagalne(W, G, L).
