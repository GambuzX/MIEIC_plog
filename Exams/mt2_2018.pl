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

    find_gurls_v2(1, MenHeights, WomenHeights, Delta, Pairs),
    extract_gurl_indexes(Pairs, GurlsIndexes),
    all_distinct(GurlsIndexes),

    length(GurlsIndexes, N),
    labeling([maximize(N)], GurlsIndexes).

    
find_gurls_v2(_, [], _, _, []).
find_gurls_v2(Iter, [MenHeight | RestMens], Gurls, Delta, [Iter-CurrGurlI | RestGurls]) :-
    element(CurrGurlI, Gurls, CurrGurlHeight),
    Diff #= MenHeight - CurrGurlHeight,
    Diff #>= 0 #/\ Diff #< Delta,
    Next is Iter+1,
    find_gurls_v2(Next, RestMens, Gurls, Delta, RestGurls).
find_gurls_v2(Iter, [_ | RestMens], Gurls, Delta, RestGurlsI) :-
    Next is Iter+1,
    find_gurls_v2(Next, RestMens, Gurls, Delta, RestGurlsI).


count_found_pairs(Rest, 0).
count_found_pairs([Curr | Rest], Total) :-
    Curr #> 0 #<=> Valid,
    Total #= NextTotal + Valid,
    count_found_pairs(Rest, NextTotal).

fill_pairs_v2(_, [], []).
fill_pairs_v2(Iter, [Gurl | RestGurls], [Iter-Gurl | RestPairs]) :-
    Gurl > 0,
    Next is Iter+1,
    fill_pairs_v2(Next, RestGurls, RestPairs).
fill_pairs_v2(Iter, [Gurl | RestGurls], RestPairs) :-
    Gurl < 0,
    Next is Iter+1,
    fill_pairs_v2(Next, RestGurls, RestPairs).

extract_gurl_indexes([], []).
extract_gurl_indexes([_-Gurl | Rest], [Gurl | RestI]) :-
    extract_gurl_indexes(Rest, RestI).