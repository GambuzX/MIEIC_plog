:- use_module(library(clpfd)).

grocery(Products) :-
    Products = [Rice, Potatoes, Spaghetii, Tuna],

    domain(Products, 0, 711),

    Rice + Potatoes + Spaghetii + Tuna #= 711,
    Rice * Potatoes * Spaghetii * Tuna #= 711 * 100 * 100 * 100,

    Potatoes #> Tuna,
    Tuna #> Rice,

    minimum(Spaghetii, Products),

    count_multiples_of_10(Products, Total),
    Total #= 2,

    labeling([], Products).

count_multiples_of_10([], 0).
count_multiples_of_10([Curr | Rest], Total) :-
    Curr mod 10 #= 0 #<=> Mult,
    Total #= NextTotal + Mult,
    count_multiples_of_10(Rest, NextTotal).

