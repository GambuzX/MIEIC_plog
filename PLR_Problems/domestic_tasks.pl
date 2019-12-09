:-use_module(library(clpfd)).


domestic_tasks(Total, Tasks) :-
    length(Tasks, 4),
    domain(Tasks, 1, 2),
    global_cardinality(Tasks, [1-2, 2-2]),

    tasks_time(Tasks, 1, Total),
    %domain([Total], 0, 1000),
    labeling([minimize(Total)], Tasks).

tasks_time([], _, 0).
tasks_time([Person | Rest], 1, Total) :-
    tasks_time(Rest, 2, RestTotal),
    (
        (Person #= 1 #/\ Total #= RestTotal + 49) #\/
        (Person #= 2 #/\ Total #= RestTotal + 45)
    ).

tasks_time([Person | Rest], 2, Total) :-
    tasks_time(Rest, 3, RestTotal),
    (
        (Person #= 1 #/\ Total #= RestTotal + 72) #\/
        (Person #= 2 #/\ Total #= RestTotal + 78)
    ).

tasks_time([Person | Rest], 3, Total) :-
    tasks_time(Rest, 4, RestTotal),
    (
        (Person #= 1 #/\ Total #= RestTotal + 43) #\/
        (Person #= 2 #/\ Total #= RestTotal + 36)
    ).

tasks_time([Person | Rest], 4, Total) :-
    tasks_time(Rest, 5, RestTotal),
    (
        (Person #= 1 #/\ Total #= RestTotal + 31) #\/
        (Person #= 2 #/\ Total #= RestTotal + 29)
    ).