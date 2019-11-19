% X entre N1 e N2
:- op(600, xfx, entre).
:- op(550, xfx, atual).
:- op(500, xfx, e).

X entre N1 e N2 :-
    X entre N1 e N2 atual N1.

X entre N1 e N2 atual X :-
    X >= N1,
    X =< N2.
X entre N1 e N2 atual Y :-
    Y >= N1,
    Y =< N2,
    Next is Y+1,
    X entre N1 e N2 atual Next.