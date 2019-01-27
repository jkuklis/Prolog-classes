:- op(500, xfx, --).

% wszerz(D, L) :- wszerzp([D|X] -- X, L).

% wszerzp(Q -- _, W) :- var(Q), !, W = [].

% wszerzp([tree(L, W, P) | Q] -- [L, P | QE], ...)

% wszerzp([nil|Q] -- QE, [W, R]) :- ...


flagaHol1(L, RWB) :- flagaHol1(L, RWB -- WB, WB -- B, B -- []).

flagaHol1([], R -- R, W -- W, B -- B).

flagaHol1([r | L], [r | R] -- KR, W, B) :- flagaHol1(L, R -- KR, W, B).

flagaHol1([w | L], R, [w | W] -- KW, B) :- flagaHol1(L, R, W -- KW, B).

flagaHol1([b | L], R, W, [b | B] -- KB) :- flagaHol1(L, R, W, B -- KB).


flagaHol2(L, F) :- flagaHol2(L, X -- X, F).

flagaHol2([], WB -- [], WB).

flagaHol2([r | L], P -- K, [r | F]) :- flagaHol2(L, P -- K, F).

flagaHol2([w | L], P -- K, F) :- flagaHol2(L, [w | P] -- K, F).

flagaHol2([b | L], P -- [b | K], F) :- flagaHol2(L, P -- K, F).
