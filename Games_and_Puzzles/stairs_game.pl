jogo_escadas(1, 1, [[1]]).
jogo_escadas(2, 2, [[1,1], [2]]).

jogo_escadas(Steps, NWays, Options) :-
    Steps >= 3,
    NextSteps1 is Steps-1,
    jogo_escadas(NextSteps1, NextWays1, Options1),
    append_val_to_each(Options1, 1, NewOptions1),
    
    NextSteps2 is Steps-2,
    jogo_escadas(NextSteps2, NextWays2, Options2),
    append_val_to_each(Options2, 2, NewOptions2),

    NWays is NextWays1 + NextWays2,
    append(NewOptions1, NewOptions2, Options).

append_val_to_each([], _, []) :- !.
append_val_to_each( [[H | T] | Rest], Val, [[Val, H | T] | Res]) :-
    append_val_to_each(Rest, Val, Res).
