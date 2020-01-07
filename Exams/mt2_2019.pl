:- use_module(library(clpfd)).

create_task(_-Volume, task(0, 1, 1, Volume, Machine), Machine).

create_machines(_, [], []).
create_machines(ID, [VPrateleira | Rest], [machine(ID, VPrateleira) | RestMachines]) :-
    NextID is ID+1,
    create_machines(NextID, Rest, RestMachines).

sum_of_div_elements([], _, _, 0).
sum_of_div_elements([CurrI | RestI], [Weight-_ | RestObjects], Div, Total) :-
    (CurrI #= Div) #<=> Between,
    Total #= Between * Weight + NextTotal,
    sum_of_div_elements(RestI, RestObjects, Div, NextTotal).

force_weights(LastDiv, LastDiv, _, _, _).
force_weights(CurrDiv, LastDiv, DivsPerRow, Vars, Objects) :-

    DivBelow is CurrDiv + DivsPerRow,

    sum_of_div_elements(Vars, Objects, CurrDiv, CurrSum),
    sum_of_div_elements(Vars, Objects, DivBelow, NextSum),
    CurrSum #=< NextSum,

    NextDiv is CurrDiv+1,
    force_weights(NextDiv, LastDiv, DivsPerRow, Vars, Objects).


prat(Prateleiras, Objetos, Vars) :-
    length(Objetos, N),
    length(Vars, N),

    append(Prateleiras, AllPrateleiras),
    length(AllPrateleiras, NDivs),

    domain(Vars, 1, NDivs),

    maplist(create_task, Objetos, Tasks, Vars),
    create_machines(1, AllPrateleiras, Machines),

    [FirstRow | _] = Prateleiras,
    length(FirstRow, K),
    LastDiv is NDivs-K+1,
    force_weights(1, LastDiv, K, Vars, Objetos),

    cumulatives(Tasks, Machines, [bound(upper)]), 
    labeling([], Vars).



objeto(piano, 3, 30).
objeto(cadeira, 1, 10).
objeto(cama, 3, 15).
objeto(mesa, 2, 15).
homens(4).
tempo_max(60).


create_tasks(_, [], [], [], []).
create_tasks(ID, [CurrStartTime | RestStartTimes], [CurrEndTime | RestEndTimes], [_-People-Duration | RestObjects], [task(CurrStartTime, Duration, CurrEndTime, People, ID) | RestTasks]) :-
    NextID is ID+1,
    create_tasks(NextID, RestStartTimes, RestEndTimes, RestObjects, RestTasks).

write_tasks([], [], []).
write_tasks([Start| StartTimes], [End | EndTimes], [Name-_-_ | Objects]) :- 
    write(Name), write(': '), write(Start-End), nl,
    write_tasks(StartTimes, EndTimes, Objects).

furniture :-
    tempo_max(Max_time),
    homens(Manpower),
    findall(Name-People-Duration, objeto(Name, People, Duration), Objects),
    length(Objects, NTasks),

    length(StartTimes, NTasks),
    length(EndTimes, NTasks),
    create_tasks(1, StartTimes, EndTimes, Objects, Tasks),
    domain(StartTimes, 0, Max_time),

    maximum(End, EndTimes),
    End #=< Max_time,

    cumulative(Tasks, [limit(Manpower)]),
    labeling([minimize(End)], StartTimes),

    write('Total time: '), write(End), nl,
    write_tasks(StartTimes, EndTimes, Objects).