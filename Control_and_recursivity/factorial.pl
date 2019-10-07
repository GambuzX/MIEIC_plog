factorial(0, 1).
factorial(1, 1).
factorial(N, Valor) :-
    N > 1,
    N1 is N-1, factorial(N1, Y),
    Valor is N * Y.    


fact2(N, F) :-
    fact_acc(N, F, 1).

fact_acc(1, F, F).

fact_acc(N, F, Acc) :-
    N > 1,
    N1 is N-1,
    Acc1 is Acc * N,
    fact_acc(N1, F, Acc1).