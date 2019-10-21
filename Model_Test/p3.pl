:- include('db.pl').

didNotPress(1, [Curr | _]) :- Curr = 120.
didNotPress(JuriMember, [Curr | Rest]) :-
    JuriMember >= 1,
    N is JuriMember-1,
    didNotPress(N, Rest).

findPerson(P, [P|_]).
findPerson(P, [H | T]) :- findPerson(P, T).

countPatience(_, 0, _).
countPatience(JuriMember, Patience, Seen) :-
    performance(P, PerfList),
    \=findPerson(P, Seen),
    (
        didNotPress(JuriMember, PerfList), NewPatience is Patience+1;
        NewPatience is Patience 
    ),
    countPatience(JuriMember, NewPatience, [P | Seen]).

patientJuri(JuriMember) :-
    performance(_, P),
    countPatience(JuriMember, P, []),
    P >= 2.
