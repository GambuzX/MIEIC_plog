:- use_module(library(clpfd)).
:- use_module(library(lists)).

magic_square_3(L) :-
    L = [C1, C2, C3, C4, C5, C6, C7, C8, C9],
    domain(L, 1, 9),
    all_distinct(L),
    C4 + C5 + C6 #= Sum,
    C7 + C8 + C9 #= Sum,
    C1 + C4 + C7 #= Sum,
    C2 + C5 + C8 #= Sum,
    C3 + C6 + C9 #= Sum,
    C1 + C5 + C9 #= Sum,
    C3 + C5 + C7 #= Sum,
    labeling([], L).

magic_square(N, L) :-
    length(L, N),
    init_list(L, N),

    Size is N*N,
    force_domain(L, Size),
    
    append(L, Appended),
    all_distinct(Appended),
    
    [First | _] = L,
    sum(First, #=, Sum),

    sum_lines(L, Sum),
    sum_cols(L, Sum),
    sum_diagonals(L, Sum),
    labeling([], Appended).

init_list([], _).
init_list([Curr | Rest], N) :-
    length(Curr, N),
    init_list(Rest, N).

force_domain([], _).
force_domain([Curr | Rest], Size) :-
    domain(Curr, 1, Size),
    force_domain(Rest, Size).

sum_lines([], _).
sum_lines([Curr|Rest], Sum) :-
    sum(Curr, #=, Sum),
    sum_lines(Rest, Sum).



sum_cols(L, Sum) :-
    sum_cols(L, 1, Sum).

sum_cols(L, Col, _) :- length(L, N), Col > N, !.
sum_cols(L, Col, Sum) :-
    sum_col(L, Col, ColVars),
    sum(ColVars, #=, Sum),

    Next is Col+1,
    sum_cols(L, Next, Sum).

sum_col([], _, []) :- !.
sum_col([CurrRow | Rest], Col, [Ele | RestVars]) :-
    nth1(Col, CurrRow, Ele),
    sum_col(Rest, Col, RestVars).



    

sum_diagonals(L, Sum) :-
    sum_diagonal_1(L, Sum),
    sum_diagonal_2(L, Sum).

sum_diagonal_1(L, Sum) :-
    sum_diagonal_1_aux(L, 1, Vars),
    sum(Vars, #=, Sum).

sum_diagonal_1_aux([], _, []).
sum_diagonal_1_aux([CurrRow | Rest], Col, [Ele | Vars]) :-
    nth1(Col, CurrRow, Ele),
    Next is Col+1,
    sum_diagonal_1_aux(Rest, Next, Vars).



sum_diagonal_2(L, Sum) :-
    sum_diagonal_2_aux(L, 1, Vars),
    sum(Vars, #=, Sum).

sum_diagonal_2_aux([], _, []).
sum_diagonal_2_aux([CurrRow | Rest], Col, [Ele | Vars]) :-
    length(CurrRow, N),
    TargetCol is N-Col+1,
    nth1(TargetCol, CurrRow, Ele),

    Next is Col+1,
    sum_diagonal_2_aux(Rest, Next, Vars).