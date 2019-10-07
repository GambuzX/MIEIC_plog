%a
delete_one(X, L, Lr) :-
    append(La, [X|Lb], L),
    append(La, Lb, Lr).

%b
delete_all(X, L, L) :- \+append(_, [X|_], L).
delete_all(X, L, Lr) :-
    delete_one(X, L, Laux),
    delete_all(X, Laux, Lr).

%c
delete_all_list([X|LX], L, Lr) :-
    delete_all(X, L, Laux),
    delete_all_list(LX, Laux, Lr).