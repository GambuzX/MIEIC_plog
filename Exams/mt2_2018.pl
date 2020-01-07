:- use_module(library(lists)).
:- use_module(library(clpfd)).

gym_pairs(MenHeights, WomenHeights, Delta, Pairs) :-
    length(MenHeights, N),
    length(GurlsI, N),
    all_distinct(GurlsI),

    find_gurls(MenHeights, WomenHeights, Delta, GurlsI),
    labeling([], GurlsI),
    fill_pairs(1, GurlsI, Pairs).

    
find_gurls([], _, _, _).
find_gurls([MenHeight | RestMens], Gurls, Delta, [CurrGurlI | RestGurls]) :-
    element(CurrGurlI, Gurls, CurrGurlHeight),
    Diff #= MenHeight - CurrGurlHeight,
    Diff #>= 0 #/\ Diff #=< Delta,
    find_gurls(RestMens, Gurls, Delta, RestGurls).

fill_pairs(_, [], []).

fill_pairs(Iter, [Gurl | RestGurls], [Iter-Gurl | RestPairs]) :-
    Next is Iter+1,
    fill_pairs(Next, RestGurls, RestPairs).



optimal_skating_pairs(MenHeights, WomenHeights, Delta, Pairs) :-
    length(MenHeights, NMens),
    length(WomenHeights, NWomen),

    length(MenPairs, NMens),
    domain(MenPairs, 0, NWomen),
    
    % each women index can only appear 0 or 1 times
    BaseCardinality = [0-X],
    domain([X], 0, NMens),
    define_cardinality(BaseCardinality, 1, WomenHeights, Cardinality),
    global_cardinality(MenPairs, Cardinality),

    count_valid_pairs(MenPairs, MenHeights, WomenHeights, Delta, Total),

    labeling([maximize(Total)], MenPairs),
    
    build_pairs(1, MenPairs, Pairs).

count_valid_pairs([], [], _, _, 0).
count_valid_pairs([CurrPair | Pairs], [MenHeight | Mens], WomenHeights, Delta, Total) :-
    element(CurrI, WomenHeights, WomenHeight),
    (
        CurrPair #> 0 #/\
        CurrI #= CurrPair #/\
        MenHeight #>= WomenHeight #/\
        MenHeight - WomenHeight #< Delta
    ) #<=> Valid,
    Total #= NextTotal + Valid,
    count_valid_pairs(Pairs, Mens, WomenHeights, Delta, NextTotal).

define_cardinality(Card, _, [], Card).
define_cardinality(Base, Iter, [_ | Rest], New) :-
    append(Base, [Iter-X], TempCard),
    domain([X], 0, 1),
    Next is Iter+1,
    define_cardinality(TempCard, Next, Rest, New).

build_pairs(_, [], []).
build_pairs(Iter, [0 | Rest], RestPairs) :-
    Next is Iter+1,
    build_pairs(Next, Rest, RestPairs).
build_pairs(Iter, [Women | Rest], [Iter-Women | RestPairs]) :-
    Women \= 0,
    Next is Iter+1,
    build_pairs(Next, Rest, RestPairs).