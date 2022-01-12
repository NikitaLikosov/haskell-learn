:- initialization(consult('kinship.pro')).

pred([Head | Tail], X) :- atom(Head), !, pred(Tail, X1), [Head | X1] = X.
pred([_ | Tail], X) :- pred(Tail, X).
pred([], []).

stepfather(X,Y):- male(X), wife(X, Z), parent(Z, Y) , \+ parent(X,Y).
wife(X, Y):- parent(X, Q) , parent(Y, Q) , female(Y), !.
w(X,Y):- male(X), female(Y), setof(Z, (parent(X, Z) , parent(Y, Z)), _).

maxNum(L,NL):-maxNumCalc(L, X), listToNum(X,NL).

listToNum([H|T],Req):- listNum(T, Req1), Req = (Req1 * 10) + H.
listToNum([],0).

maxNumCalc(L,NL):- max_list(L, X), deleteCalc(L,X,L1), maxNumCalc(L1, NL1), NL = [X | NL1].
maxNumCalc([], []).

deleteCalc([Head | Tail], X, Nl) :- Head = X,print(Head), !, Tail = Nl.
deleteCalc([Head | Tail], X, Nl) :- deleteCalc(Tail, X, Nl1), Nl = [Head | Nl1].
deleteCalc([], [], Nl).


% maxCalc([H|T], X, NewList):- nonvar(X), H < X, maxCalc(T, X, NewList).
% maxCalc([H|T], X, NewList):- var(X), X = H, maxCalc(T, H, NewList).
% maxCalc([H|T], X, NewList):- maxCalc(T, X, NewList1), [H |NewList1] = NewList.
% maxCalc([], X, NewList):- nonvar(X).