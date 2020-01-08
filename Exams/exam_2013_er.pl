:- use_module(library(clpfd)).

regioes(N, ListaAdjacencias, Regioes) :-
    length(Regioes, N),
    domain(Regioes, 1, N),

    different_colors(ListaAdjacencias, Regioes),
    pair_values(ListaAdjacencias, Regioes, 3),
    get_cost(1, Regioes, Cost),

    labeling([minimize(Cost)], Regioes).

different_colors([], _).
different_colors([I1-I2 | Adj], Regioes) :-
    nth1(I1, Regioes, Reg1),
    nth1(I2, Regioes, Reg2),
    Reg1 #\= Reg2,
    different_colors(Adj, Regioes).

pair_values([], _, 0).
pair_values([I1-I2 | Adj], Regioes, Total) :-
    nth1(I1, Regioes, Reg1),
    nth1(I2, Regioes, Reg2),
    
    (Reg1 mod 2 #= 0 #/\ Reg2 mod 2 #= 0) #<=> Pair,
    Total #= NextTotal + Pair,
    pair_values(Adj, Regioes, NextTotal).

get_cost(_, [], 0).
get_cost(Iter, [Color | Rest], Cost) :-
    Cost #= NextCost + Iter - Color + 1,
    Next is Iter+1,
    get_cost(Next, Rest, NextCost).