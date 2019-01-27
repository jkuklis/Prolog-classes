:- op(500, xfx, --).

flagaPolska(L, F) :- flagaPolska(L, X -- X, F).

flagaPolska([], P -- [], P).

flagaPolska([c | L], P -- [c | K], F) :- flagaPolska(L, P -- K, F).

flagaPolska([b | L], LR, [b | F]) :- flagaPolska(L, LR, F).


flagaPolska2(L, F) :- flagaPolska2H(L, F -- []).

flagaPolska2H([], X -- X).

flagaPolska2H([b | L], [b | P] -- K) :- flagaPolska2H(L, P -- K).

flagaPolska2H([c | L], P -- K) :- flagaPolska2H(L, P -- [c | K]).


flagaHol(L, RWB) :- flagaHol(L, X -- X, RWB).

flagaHol([], WB -- [], WB).

flagaHol([r | L], LR, [r | F]) :- flagaHol(L, LR, F).

flagaHol([w | L], WB -- B, F) :- flagaHol(L, [w | WB] -- B, F).

flagaHol([b | L], WB -- [b | B], F) :- flagaHol(L, WB -- B, F). 
