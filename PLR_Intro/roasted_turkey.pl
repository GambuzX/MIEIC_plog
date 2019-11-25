
turkey(Price, Total) :-
    Vars = [D1, D2],
    domain(Vars, 1, 9),
    domain([Price], 1, 999999),
    domain([Total], 1, 999999),

    Total #= D1*1000 + 670 + D2,
    Price*72 #= Total,
    labeling([], [D1, D2, Price, Total]).