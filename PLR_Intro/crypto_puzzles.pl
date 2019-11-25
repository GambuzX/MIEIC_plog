:-use_module(library(clpfd)).

puzzle(3, Letters) :-
    Letters = [S, E, N, D, M, O, R, Y],
    domain(Letters, 0, 9),
    all_different(Letters),

    S #\= 0, M #\= 0,

    S*1000 + E*100 + N*10 + D + M*1000 + O*100 + R*10 + E #= M*10000 + O*1000 + N*100 + E*10 + Y,
    labeling([], Letters).