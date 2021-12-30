:- initialization(consult('kinship.pro')).

married(X, Y) :-
    bagof(A, (parent(X, A), parent(Y, A)), Res),
    X \== Y,
    Res \== [].

stepfather(X, Y) :- 
    setof(Z, (male(X), female(Z), parent(X, Z), married(X, Z), \+ parent(X, Y)), _).