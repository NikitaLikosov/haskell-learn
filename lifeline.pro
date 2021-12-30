:- initialization(consult('kinship.pro')).

pred([Head | Tail], X) :- atom(Head), !, pred(Tail, X1), [Head | X1] = X.
pred([_ | Tail], X) :- pred(Tail, X).
pred([], []).

stepfather(X,Y):- male(X), wife(X, Z), parent(Z, Y) , \+ parent(X,Y).
wife(X, Y):- parent(X, Q) , parent(Y, Q) , female(Y), !.
w(X,Y):- male(X), female(Y), setof(Z, (parent(X, Z) , parent(Y, Z)), _).
