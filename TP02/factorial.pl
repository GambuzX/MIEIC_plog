factorial(1, 1).
factorial(N, Valor) :-
    N > 1,
    N1 is N-1, factorial(N1, Y),
    Valor is N * Y.    