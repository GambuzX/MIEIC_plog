
salto_cavalo(X1/Y1, X2/Y2) :-
    (
        X2 is X1-2, Y2 is Y1-1;
        X2 is X1-2, Y2 is Y1+1;
        X2 is X1+2, Y2 is Y1-1;
        X2 is X1+2, Y2 is Y1+1;
        X2 is X1-1, Y2 is Y1-2;
        X2 is X1+1, Y2 is Y1-2;
        X2 is X1-1, Y2 is Y1+2;
        X2 is X1+1, Y2 is Y1+2
    ),
    X2 >= 1, X2 =< 8,
    Y2 >= 1, Y2 =< 8.

trajecto_cavalo([_]).
trajecto_cavalo([Curr, Next | Rest]) :-
    salto_cavalo(Curr, Next),
    trajecto_cavalo([Next | Rest]).