:-use_module(library(clpfd)).

color(1, yellow).
color(2, green).
color(3, blue).
color(4, black).

car_line(FirstCar) :-
    Positions = [YellowP, GreenP, BlueP, BlackP],
    Sizes = [YellowS, GreenS, BlueS, BlackS],
    domain(Positions, 1, 4),
    domain(Sizes, 1, 4),
    all_distinct(Positions),
    all_distinct(Sizes),

    GreenP #> BlueP,
    YellowP #> BlackP,

    GreenS #= 1,

    BeforeBlue #= BlueP-1,
    AfterBlue #= BlueP+1,

    element(BeforeIndex, Positions, BeforeBlue),
    element(BeforeIndex, Sizes, BeforeBlueSize),

    element(AfterIndex, Positions, AfterBlue),
    element(AfterIndex, Sizes, AfterBlueSize),
    BeforeBlueSize #< AfterBlueSize,

    append(Positions, Sizes, AllVars),
    labeling([], AllVars),

    element(FirstCarI, Positions, 1),
    color(FirstCarI, Color),
    FirstCar = Color.


    