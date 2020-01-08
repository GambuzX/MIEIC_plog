:- use_module(library(clpfd)).
:- use_module(library(lists)).

sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs) :-

    length(RecipeTimes, N),
    length(ChoosenRecipes, N),
    domain(ChoosenRecipes, 0, 1),

    scalar_product(RecipeTimes, ChoosenRecipes, #=<, MaxTime),
    scalar_product(RecipeEggs, ChoosenRecipes, #=, Eggs),

    Eggs #=< NEggs,

    exactly(1, ChoosenRecipes, 3),

    labeling([maximize(Eggs)], ChoosenRecipes),

    findall(I, nth1(I, ChoosenRecipes, 1), Cookings).


exactly(_, [], 0).
exactly(X, [Y|L], N) :-
    X #= Y #<=> B,
    N #= M + B,
    exactly(X, L, M).


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


make_item(Shelf, item(Bin, Shelf), Bin).
make_bins(_, [], []).
make_bins(ID, [Board | Rest], [bin(ID, Cap) | Bins]) :-
    NextID is ID+1,
    Cap in 0..Board,
    make_bins(NextID, Rest, Bins).

cut_bin_packing(Shelves, Boards, SelectedBoards) :-
    length(Shelves, N),
    length(SelectedBoards, N),

    maplist(make_item, Shelves, Items, SelectedBoards),
    make_bins(1, Boards, Bins),

    bin_packing(Items, Bins),

    labeling([], SelectedBoards).
