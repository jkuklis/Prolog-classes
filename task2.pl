neighbours(a, [b,c,d]).
neighbours(b, [e]).
neighbours(c, [d]).
neighbours(d, [a]).
neighbours(e, [e]).
 
edge(A, B) :- neighbours(A, L), member(B, L).
 
 
pathC(A, B) :- pathC(A, B, [A]).
 
pathC(A, B, _) :- edge(A, B).
pathC(A, B, V) :-
    edge(A, C),
    \+ member(C, V),
    pathC(C, B, [C | V]).
 
 
cykle(V, W) :- cykle(V, [], W).
 
cykle([], W, W).
cykle([X | V], A, W) :- pathC(X, X), !, cykle(V, [X | A], W).
cykle([_ | V], A, W) :- cykle(V, A, W).
