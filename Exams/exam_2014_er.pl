hours([7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5]).

airTime('The Walking Dead',sunday,9).
airTime('Game of Thrones',sunday,8.5).
airTime('The Big Bang Theory',monday,8).
airTime('How I Met Your Mother',thursday,8).
airTime('Mad Men',sunday,10).

views('The Walking Dead',11).
views('Game of Thrones',5).
views('The Big Bang Theory',9).
views('Mad Men',2.5).
views('How I Met Your Mother',19).

network('The Walking Dead',amc).
network('Mad Men',amc).
network('Game of Thrones',hbo).
network('The Big Bang Theory',cbs).
network('How I Met Your Mother',cbs).

tvShowNetwork(Network, DayOfWeek, Hour, TvShow) :-
    network(TvShow, Network),
    airTime(TvShow, DayOfWeek, Hour).

mostViews(Network, TvShow, DayOfWeek, Hour) :-
    network(TvShow, Network),
    airTime(TvShow, DayOfWeek, Hour),
    \+betterShow(TvShow, Network, _).

betterShow(TvShow1, Network, TvShow2) :-
    network(TvShow2, Network),
    TvShow2 \= TvShow1,
    views(TvShow1, Views1),
    views(TvShow2, Views2),
    Views2 > Views1.


hottestTvShows([], []).
hottestTvShows([Curr | Networks], [CurrBest | TvShows]) :-
    mostViews(Curr, CurrBest, _, _),
    hottestTvShows(Networks, TvShows).


schedule(Network, DayOfWeek, Schedule) :-
    hours(Hours),
    scheduleAux(Network, DayOfWeek, Hours, Schedule).

scheduleAux(_, _, [], []) :- !.
scheduleAux(Network, DayOfWeek, [CurrHour | NextHour], [CurrHour-TvShow | Schedule]) :-
    network(TvShow, Network),
    airTime(TvShow, DayOfWeek, CurrHour),
    scheduleAux(Network, DayOfWeek, NextHour, Schedule), !.

scheduleAux(Network, DayOfWeek, [_ | NextHour], Schedule) :-
    scheduleAux(Network, DayOfWeek, NextHour, Schedule), !.


averageViewers(Network, DayOfWeek, Average) :-
    schedule(Network, DayOfWeek, Schedule),
    viewsList(Schedule, ViewsList, SumViews),
    length(ViewsList, NShows),
    (
        NShows = 0, Average = 0;
        Average is SumViews/NShows
    ), !.

viewsList([], [], 0).
viewsList([_-TvShow | Rest], [CurrViews | ViewsList], SumViews) :-
    views(TvShow, CurrViews),
    viewsList(Rest, ViewsList, RestSumViews),
    SumViews is RestSumViews+CurrViews.

project(projA,qwe).

task(projA,t1,a,3).
task(projA,t2,b,2).
task(projA,t3,c,4).
task(projA,t4,d,2).

precedence(projA,t1,t2).
precedence(projA,t1,t3).
precedence(projA,t2,t4).
precedence(projA,t3,t4).

proj_tasks(L) :-
    findall(ProjId, project(ProjId, _), ListIds),
    assignTasks(ListIds, L).

assignTasks([], []).
assignTasks([Curr | RestIDs], [Curr-NTasks | OutList]) :-
    findall(TaskId, task(Curr, TaskId, _, _), TaskList),
    length(TaskList, NTasks),
    assignTasks(RestIDs, OutList).

p(X,P,D) :- 
    task(X,Y,_,_), 
    \+ precedence(X,_,Y), 
    p(X,Y,P,D).

p(X,Y,[Y|P],D) :- 
    precedence(X,Y,Z), 
    task(X,Y,_,D1),
    p(X,Z,P,D2), 
    D is D1+D2.
p(X,Y,[Y],D) :- 
    \+ precedence(X,Y,_), 
    task(X,Y,_,D).


table6(L) :-
    Position = [Asd, Ber, Cris, Dem, Ele, Fel],
    domain(Positions, 1, 6),
    all_distinct(Positions),

    Ber #= (Asd + 1) mod 6 + 1 #\/ Ber #= (Asd - 1) mod 6 + 1.
