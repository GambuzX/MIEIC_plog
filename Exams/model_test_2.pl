:- use_module(library(lists)).
:- use_module(library(clpfd)).

p1(L1,L2) :-
    gen(L1,L2),
    test(L2).

gen([],[]).
gen(L1,[X|L2]) :-
    select(X,L1,L3),
    gen(L3,L2).

test([_,_]).
test([X1,X2,X3|Xs]) :-
    (X1 < X2, X2 < X3; X1 > X2, X2 > X3),
    test([X2,X3|Xs]).

p2(L1,L2) :-
    length(L1,N),
    length(L2,N),
    %
    pos(L1,L2,Is),
    all_distinct(Is),
    %
    labeling([],Is),
    test(L2).

pos([],_,[]).
pos([X|Xs],L2,[I|Is]) :-
    nth1(I,L2,X),
    pos(Xs,L2,Is).

p3(L1, L2) :-
    length(L1, N),
    length(L2, N),

    all_distinct(L2),
    list_to_fdset(L1, L1_Set),
    force_members(L2, L1_Set),

    test_plr(L2),
    labeling([], L2).

force_members([], _).
force_members([Curr | Rest], L1) :-
    Curr in_set L1,
    force_members(Rest, L1).

test_plr([_,_]).
test_plr([X1,X2,X3|Xs]) :-
    (X1 #< X2 #/\ X2 #< X3) #\/
    (X1 #> X2 #/\ X2 #> X3),
    test_plr([X2, X3|Xs]).


build(Budget, NPacks, ObjectsCosts, ObjectPacks, Objects, UsedPacks) :-
    length(ObjectsCosts, NObjects),

    % indexes
    length(Objects, 3),
    domain(Objects, 1, NObjects),
    [I1, I2, I3] = Objects,
    all_distinct(Objects),

    element(I1, ObjectsCosts, I1Cost),
    element(I1, ObjectPacks, I1Packs),
    element(I2, ObjectsCosts, I2Cost),
    element(I2, ObjectPacks, I2Packs),
    element(I3, ObjectsCosts, I3Cost),
    element(I3, ObjectPacks, I3Packs),

    I1Cost + I2Cost + I3Cost #=< Budget,
    UsedPacks #= I1Packs + I2Packs + I3Packs,
    UsedPacks #=< NPacks,

    labeling([maximize(UsedPacks)], Objects).

wrap(Presents, PaperRolls, SelectedPaperRolls) :-
    length(Presents, N),
    length(PaperRolls, NPaperRolls),
    length(SelectedPaperRolls, N),

    domain(SelectedPaperRolls, 1, NPaperRolls),

    verify_sums(1, PaperRolls, SelectedPaperRolls, Presents),
    labeling([], SelectedPaperRolls).

verify_sums(_, [], _, _).
verify_sums(CurrID, [Amount | Rest], Selected, Presents) :-
    check_sum(Selected, Presents, CurrID, Total),
    Total #=< Amount,
    NextID is CurrID+1,
    verify_sums(NextID, Rest, Selected, Presents).

check_sum([], [], _, 0).
check_sum([CurrIndex | RestIndex], [CurrPresent | RestPresents], ID, Total) :-
    (CurrIndex #= ID) #<=> Equal,
    Total #= Equal * CurrPresent + RestTotal,
    check_sum(RestIndex, RestPresents, ID, RestTotal).