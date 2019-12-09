:-use_module(library(clpfd)).

lazy_mailman(Time) :-
    %Houses = [H1, H2, H3, H4, H5, H6, H7, H8, H9, H10],
    length(Houses, 10),
    domain(Houses, 1, 10),
    all_distinct(Houses),
    element(10, Houses, 6),


    houses_dist(Houses, Time),
    Time #>= 45,
    labeling([maximize(Time)], [Time | Houses]).

houses_dist([_], 0).
houses_dist([Curr, Next | Rest], Sum) :-
    Diff #= abs(Next - Curr),
    houses_dist([Next|Rest], RestSum),
    Sum #= RestSum + Diff.