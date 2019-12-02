:-use_module(library(clpfd)).

color(1, amarelo).
color(2, verde).
color(3, vermelho).
color(4, azul).
another_car_line(Line) :-
    Line = [C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12],
    domain(Line, 1, 4),

    global_cardinality(Line, [1-4, 2-2, 3-3, 4-3]),
    C1 #= C12,
    C2 #= C11,
    C5 #= 4,

    all_distinct_pairs_of_three(Line),
    count_pattern(Line, 1),

    labeling([], Line).


all_distinct_pairs_of_three([_, _ | []]).
all_distinct_pairs_of_three([C1, C2, C3 | Rest]) :-
    all_distinct([C1,C2,C3]),
    all_distinct_pairs_of_three([C2, C3 | Rest]).

count_pattern([_, _, _ | []], 0).
count_pattern([C1, C2, C3, C4 | Rest], Count) :-
    (C1 #= 1 #/\ C2 #= 2 #/\ C3 #= 3 #/\ C4 #= 4) #<=> B,
    Count #= N + B,
    count_pattern([C2, C3, C4 | Rest], N).

