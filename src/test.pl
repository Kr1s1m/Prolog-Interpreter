male(jawor).
male(kris).
male(  koce   ) .
male(  gosho).

parent(jawor,  martina   ).
parent(jawor,    kris     ) .

parent(koce, daniela).
parent(gosho, jawor).

parent(daniela  ,kris) .
parent(daniela,martina).

grandfather(X,Y) :-  male(X),  parent(X,Z),  parent(Z,Y)  .

nat(s).
nat( d(X) ) :- nat(X).