% se X > Y entao Z := X senao Z := Y.
:- op(900, fx, se).
:- op(850, xfx, entao).
:- op(800, xfx, senao).
:- op(100, xfx, :=).

se A entao B senao C :- A, !, B.
se A entao B senao C :- C.

A := B :- A = B.