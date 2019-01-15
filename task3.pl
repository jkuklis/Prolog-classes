cykle([], []).
cykle([VH | VT], [VH | W]) :- cykl(VH), !, cykle(VT, W).
cykle([_ | VT], W) :- cykle(VT, W).
 
cykl(V) :- cykl(V, [V], _, 0, 1).
 
% cykl(Root, VisitedAcc, Reachable, IsRootReachableAcc, IsRootReachable)
cykl(V, [W | WT], R, BA, BR) :- neighbours(W, UL), cykl(V, UL, [W | WT], R, BA, BR).
 
% cykl(Root, Neighbours, VisitedAcc,Reachable, IsRootReachableAcc, IsRootReachable)
cykl(_, [], A, A, BA, BA).
cykl(V, [V | TU], A, R, _, 1) :- cykl(V, TU, A, R, 1, 1).
cykl(V, [U | TU], A, R, BA, BR) :- V \= U, member(U, A), cykl(V, TU, A, R, BA, BR).
cykl(V, [U | TU], A, R, BA, BR) :-
    \+ member(U, A),
    cykl(V, [U | A], R1, BA, BA2),
    cykl(V, TU, R1, R, BA2, BR).
