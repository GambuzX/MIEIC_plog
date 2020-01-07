:- use_module(library(clpfd)).
:- use_module(library(lists)).

concelho(x, 120, 410).
concelho(y, 10, 800).
concelho(z, 543, 2387).
concelho(k, 3, 38).
concelho(w, 234, 376).

get_dist(_-D-_, D).

concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores) :-
    
    % get all locations
    findall(Name-Dist-NPeople, concelho(Name, Dist, NPeople), Concelhos),
        
    length(Concelhos, NLocations), % total number of locations

    % list of 1's and 0's that signal whether each location was visited
    length(Visited, NLocations),
    domain(Visited, 0, 1),

    % can only visited at most MaxVisitedDays places
    exactly(1, Visited, TotalVisited),
    TotalVisited #=< NDias,

    % get distances from each location
    maplist(get_dist, Concelhos, Distances),

    % impose limit on distance
    scalar_product(Distances, Visited, #=, DistTotal),
    DistTotal #=< MaxDist,

    % count electors
    count_electors(Concelhos, Visited, TotalEleitores),
    labeling([maximize(TotalEleitores)], Visited),

    get_locations(Concelhos, Visited, ConcelhosVisitados).

count_electors([], [], 0).
count_electors([_-_-NPeople | Concelhos], [Visited | Rest], Total) :-
    Total #= NextTotal + Visited * NPeople,
    count_electors(Concelhos, Rest, NextTotal).

exactly(_, [], 0).
exactly(X, [Y|L], N) :-
    X #= Y #<=> B,
    N #= M+B,
    exactly(X, L, M).

get_locations([], [], []).
get_locations([Name-_-_ | Concelhos], [1 | Rest], [Name | RestVisited]) :-
    get_locations(Concelhos, Rest, RestVisited).
get_locations([Name-_-_ | Concelhos], [0 | Rest], Visited) :-
    get_locations(Concelhos, Rest, Visited).

