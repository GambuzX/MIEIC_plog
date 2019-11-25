:- use_module(library(clpfd)).

guards(Rooms) :-
    Rooms = [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12],
    domain(Rooms, 0, 12),

    R1 + R2 + R3 + R4 #= 5,
    R4 + R5 + R6 + R7 #= 5,
    R7 + R8 + R9 + R10 #= 5,
    R10 + R11 + R12 + R1 #= 5,
    
    sum(Rooms, #=, 12),

    labeling([], Rooms),
    write(Rooms).
