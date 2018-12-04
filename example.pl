/**

:- op(500, xfy, .).

lista(nil).

lista(_.Y) :- lista(Y).

pierwszy(E, E._).

ostatni(E, E.nil).

ostatni(E, _.Y.L) :- ostatni(E, Y.L).

element(E, E._).

element(E, _.Y.L) :- element(E, Y.L).

*/

pierwszy(E, [E|_]).

ostatni(E, [E]).

ostatni(E, [_,Y|L]) :- ostatni(E, [Y|L]).

element(E, [E|_]).

element(E, [_,X|L]) :- element(E, [X|L]).

intersection([E|_], L) :- element(E, L).

intersection([_|L], L2) :- intersection(L, L2).

scal([], L, L).

scal([X|L1], L2, [X|L3]) :- scal(L1, L2, L3).

podwojona(L, L2) :- scal(L, L, L2).

prefix(Pref, L) :- scal(Pref, _, L).

suffix(Suf, L) :- scal(_, Suf, L).

prefixosuffix(PS, L) :- scal(PS, L1, L), scal(_, PS, L1).


podziel(_, [], [], []).

podziel("p", [X|L], [X|N], P) :- podziel("niep", L, N, P).

podziel("niep", [X|L], N, [X|P]) :- podziel("p", L, N, P).

podziel(Lista, NieParz, Parz) :- podziel("p", Lista, NieParz, Parz).



podziel2([X,Y|L], [X|N], [Y|P]) :- podziel2(L, N, P).

podziel2([X], [X], []).

podziel2([], [], []).


podziel3([X|L], [X|N], P) :- podziel3(L, P, N).

podziel3([], [], []).


p(a). p(b). p(a).


wypisz([]) :- write('Lista pusta').

wypisz([X]) :- write(X), write('.').

wypisz([X,Y|L]) :- write(X), write(', '), wypisz([Y|L]).


podlista(P, L) :- scal(_, L1, L), scal(P, _, L1).


podlista2(P, L) :- append(P, _, L).

podlista2(P, [_|L]) :- podlista2(P, L).


podlista3([], _).

podlista3([X|P], [X|L]) :- append(P, _, L).

podlista3([X|P], [_|L]) :- podlista3([X|P], L).


podciag([], _).

podciag([X|P], [X|L]) :- podciag(P, L).

podciag([X|P], [_|L]) :- podciag([X|P], L).


tuzPrzed(X, Y, [X,Y|_]).

tuzPrzed(X, Y, [_|L]) :- tuzPrzed(X, Y, L).


przed(X, Y, [X|L]) :- member(Y, L).

przed(X, Y, [_|L]) :- przed(X, Y, L).


tuzPrzed2(X, Y, L) :- append(_, [X,Y|_], L).


przed2(X, Y, L) :- podciag([X,Y], L).


przed3(X, Y, L) :- append(_, [X|T], L), member(Y, T).


odwrotna([], []).

odwrotna([X|L], R) :- odwrotna(L, RT), append(RT, [X], R).


odwrotna2(L, R) :- odwrotna2_h(L, R, []).

odwrotna2_h([], R, R).

odwrotna2_h([X|L], R, A) :- odwrotna2_h(L, R, [X|A]).


palindrom(W) :- odwrotna2(W, W).


palindromParzysty(W) :- append(L, R, W), odwrotna2(L, R).

palindromNieparzysty(W) :- append(L, [_|R], W), odwrotna2(L,R).


palindrom2(W) :- palindromParzysty(W).

palindrom2(W) :- palindromNieparzysty(W).


palindrom3_h([], W, W).

palindrom3_h([X|L], W, A) :- palindrom3_h(L, W, [X|A]).

palindrom3(W) :- palindrom3_h(W, W, []).


sufiksy([], [[]]).

sufiksy([X|L], [[X|Y],Y|S]) :- sufiksy(L, [Y|S]).


flaga_polska(L, F) :- flaga_polska(L, [], [], F).

flaga_polska([], B, C, F) :- append(B, C, F).

flaga_polska([b|L], B, C, F) :- flaga_polska(L, [b|B], C, F).

flaga_polska([c|L], B, C, F) :- flaga_polska(L, B, [c|C], F).


flaga_polska2(L, F) :- flaga_polska2(L, [], F).

flaga_polska2([], C, C).

flaga_polska2([c|L], C, F) :- flaga_polska2(L, [c|C], F).

flaga_polska2([b|L], C, [b|F]) :- flaga_polska2(L, C, F).


flaga_polska3(L, F) :- kolory(L, B, C), append(B, C, F).

kolory([], [], []).

kolory([b|L], [b|B], C) :- kolory(L, B, C).

kolory([c|L], B, [c|C]) :- kolory(L, B, C).


uzg([_]).

uzg([X,X|S]) :- uzg([X|S]).

uzg([X|T], X) :- uzg([X|T]).


uzg2(S, X) :- uzg2([X|S]).


najwyzej2([]).

najwyzej2([X|L]) :- najwyzej2(L,[X,_]).

najwyzej2([],_).

najwyzej2([X|T],L) :- member(X,L), najwyzej2(T,L).


rozna([]).

rozna([X|L]) :- unify_with_occurs_check(X, L), rozna(L).


nawiasy(0'(, 0')).

nawiasy(0'[, 0']).


wyrNawiasowe(L) :- wyrNawiasowe(L, []).

wyrNawiasowe([], []).

wyrNawiasowe([OP | L], S) :- nawiasy(OP, CP), wyrNawiasowe(L, [CP | S]).

wyrNawiasowe([CP | L], (CP | S)) :- wyrNawiasowe(L, S).


slowoX(S) :- slowoX(S, []).

slowoX([b|S], [b|S]).

slowoX([a|S], B) :- slowoX(S, [b|B]).



liczba(zero).

liczba(s(X)) :- liczba(X).


dlugosc([], zero).

dlugosc([_|L], s(K)) :- dlugosc(L, K).


dlugosc2([], 0).

dlugosc2([_|L], K) :- dlugosc2(L, K2), K is K2 + 1.


suma([], 0).

suma([X|L], K) :- suma(L, N), K is X + N.


fib(0, 1).

fib(1, 1).

fib(K, N) :- fib(K1, N1), K is K1 + 1, fib(K2, N2), K is K2 + 2, N is N1 + N2.


fib2(K, N) :- fib2(K, N, _).

fib2(0, 1, 0).

fib2(1, 1, 1).

fib2(K, N, N1) :- fib2(K1, N1, N2), K is K1 + 1, N is N2 + N1.


fib3(K, N) :- fib3(0, 1, K, N).

fib3(1, 1, 0, 1).

%fib3(FN, FN1, K, FN2) :- fib3(FN1, FN1p, Kp, FN2p), FN1p is FN + FN1, K is Kp + 1.


fib4(K, F) :- fib4(0, 0, 1, K, F).

fib4(K, F, _, K, F).

fib4(I, FI, FI1, K, F) :- 
	(integer(K) -> 
		K > I; 
		integer(F) ->
			F >= FI1;
			true),
       	I1 is I + 1, 
	FI2 is FI + FI1, 
	fib4(I1, FI1, FI2, K, F).


poziom(LL, P, LP) :- poziom(0, LL, [], [], P, LP).

poziom(P, LP, [], _, P, LP).

poziom(I, _, [], L, P, LP) :- I1 is I + 1, poziom(I1, [], L, [], P, LP).

poziom(I, LI, [X|L], LH, P, LP) :-
	integer(I) ->
		I > -1;
		integer(X) ->
			poziom(I, [X|LI], L, LH, P, LP);
			poziom(I, LI, L, [X|LH], P, LP).


exampleTree(T) :- T = tree(tree(nil, 2, nil), 5, tree(tree(nil,7,nil), 9, nil)).


drzewo(nil).

drzewo(tree(L, _, P)) :- drzewo(L), drzewo(P).


ileW(nil, 0).

ileW(tree(L, _, P), K) :- ileW(L, KL), ileW(P, KP), K is KL + KP + 1.


ileW2(T, K) :- ileW2(T, 0, K).

ileW2(nil, A, A).

ileW2(tree(L, _, P), A, W) :- A1 is A + 1, ileW2(L, A1, W1), ileW2(P, W1, W).


depth(T, K) :- depth(T, 0, 0, K).

depth(nil, AD, MA, R) :- R is max(AD, MA).

depth(tree(L, _, P), AD, MA, R) :- AD1 is AD + 1, depth(L, AD1, MA, R1), depth(P, AD1, R1, R).


insertBST(nil, E, tree(nil, E, nil)).

/* insertBST(tree(L, W, P), W, tree(L, W, P)) :- !.
*/

insertBST(tree(L, W, P), E, tree(L1, W, P)) :- W > E, !, insertBST(L, E, L1).

insertBST(tree(L, W, P), E, tree(L, W, P1)) :- W < E, insertBST(P, E, P1).


createBST(L, T) :- createBST(L, nil, T).

createBST([], T, T).

createBST([X|L], T1, T) :- insertBST(T1, X, T2), createBST(L, T2, T).


wypiszBST(T, L) :- wypiszBST(T, [], L).

wypiszBST(nil, L, L).

wypiszBST(tree(L, W, P), Li1, Li) :- wypiszBST(P, Li1, Li2), wypiszBST(L, [W|Li2], Li).


sortBST(L, S) :- createBST(L, T), wypiszBST(T, S).


liscie(nil, []).

liscie(T, L) :- liscie(T, [], L).

liscie(tree(nil, W, nil), L, [W|L]).

liscie(tree(nil, _, P), Li, R) :- liscie(P, Li, R).

liscie(tree(L, _, nil), Li, R) :- liscie(L, Li, R).

liscie(tree(L, _, P), A, R) :- liscie(P, A, R1), liscie(L, R1, R).


liscie2(T, L) :- liscie2(T, [], L).

liscie2(nil, L, L).

liscie2(tree(nil, W, nil), L, [W|L]) :- !.

/* liscie2(tree(L, _, P), Li1, Li) :- (L \= nil; P \= nil), liscie2(P, Li1, Li2), liscie2(L, Li2, Li).
*/

liscie2(tree(L, _, P), Li1, Li) :- (L, P) \= (nil, nil), liscie2(P, Li1, Li2), liscie2(L, Li2, Li).


przedstaw(W, L) :- rozdziel(W, U, D), odwrotna(U, RU), przedstaw(RU, D, L).

rozdziel([X|W], [], [X|W]) :- X >= 0.

rozdziel([X|W], [X|U], D) :- X < 0, rozdziel(W, U, D).

przedstaw([], D, D).

przedstaw(U, [], U).

przedstaw([X|U], [Y|D], [X|L]) :- X + Y >= 0, przedstaw(U, [Y|D], L).

przedstaw([Y|U], [X|D], [X|L]) :- X + Y =< 0, przedstaw([Y|U], D, L).


podziel4([], []).

podziel4([X|L], [[X]|P]) :- podziel4(L,P).

podziel4([X|L], [[X|F]|P]) :- podziel4(L, X, [F|P]).

podziel4([X|L], X, [[X]|P]) :- podziel4(L, P).

podziel4([Y|L], X, [[Y|F]|P]) :- podziel4(L, X, [F|P]).


zbierzDane2(D, LI, LS) :- zbierzDane(D, W), rozdzielWynik(W, LI, LS).

zbierzDane(D, W) :- zbierzDane(D, [], W).

zbierzDane(nil, I, I).

zbierzDane(tree(L, W, R), I, WI) :-
	zbierzDane(L, I, I1),
	dodaj(W, I1, I2),
	zbierzDane(R, I2, WI).

dodaj(d(Ident, Waga), [], [(Ident, Waga, 1)]).

dodaj(d(Ident, Waga), [(Ident, W1, K1)|L], [(Ident, W2, K2)|L]) :- !, K2 is K1 + 1, W2 is W1 + Waga.

dodaj(d(Ident, Waga), [W|L1], [W|L2]) :- dodaj(d(Ident, Waga), L1, L2).

rozdzielWynik([], [], []).

rozdzielWynik([(_, _, 1) | W], LI, LS) :- rozdzielWynik(W, LI, LS).

rozdzielWynik([(Ident, Suma, Ile) | W], [Ident|LI], [Suma|LS]) :- Ile > 1, rozdzielWynik(W, LI, LS).


exampleTree2(tree(tree(nil, d(2,3), tree(nil, d(2,5), nil)), d(4,3), tree(nil, d(1,3), tree(nil, d(4,1), nil)))).

