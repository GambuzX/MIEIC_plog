:- use_module(library(clpfd)).

make_task(CurrShelf, task(0, 1, 1, CurrShelf, Board), Board).

create_machines(_, [], []).
create_machines(ID, [CurrBoard | RestBoards], [machine(ID, CurrBoard) | RestMachines]) :-
    NextID is ID+1,
    create_machines(NextID, RestBoards, RestMachines).

cut(Shelves, Boards, SelectedBoards) :-
    length(Shelves, N),
    length(TaskList, N),
    maplist(make_task, Shelves, TaskList, SelectedBoards),
    create_machines(1, Boards, MachinesList),
    cumulatives(TaskList, MachinesList, [bound(upper)]),
    labeling([], SelectedBoards).

