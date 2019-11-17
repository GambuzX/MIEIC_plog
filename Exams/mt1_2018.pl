:- use_module(library(lists)).
:- use_module(library(sets)).

%airport(Name, ICAO, Country)
airport('Aeroporto Francisco Sá Carneiro', 'LPPR', 'Portugal').
airport('Aeroporto Humberto Delgado', 'LPPT', 'Portugal').
airport('Aeropuerto Adolfo Suárez Madrid-Barajas', 'LEMD', 'Spain').
airport('Aéroport de Pairs-Charles-de-Gaulle Roissy Airport', 'LFPG', 'France').
airport('Aeroporto Internazionale di Roma-Fiumcino - Leonardo da Vinci', 'LIRF', 'Italy').

%company(ICAO, Name, Year, Country)
company('TAP', 'TAP Air Portugal', 1945, 'Portugal').
company('RYR', 'Ryanair', 1984, 'Ireland').
company('AFR', 'Société Air France, S.A', 1933, 'France').
company('BAW', 'British Airways', 1974, 'United Kingdom').

%flight(Designation, Origin, Destination, DepartureTime, Duration, Company)
flight('TP1923', 'LPPR', 'LPPT', 1115, 55, 'TAP').
flight('TP1968', 'LPPT', 'LPPR', 2235, 55, 'TAP').
flight('TP842', 'LPPT', 'LIRF', 1450, 195, 'TAP').
flight('TP843', 'LIRF', 'LPPT', 1935, 195, 'TAP').
flight('TP5483', 'LPPR', 'LEMD', 630, 105, 'RYR').
flight('TP5484', 'LEMD', 'LPPR', 1935, 105, 'RYR').
flight('AF1024', 'LFPG', 'LPPT', 940, 155, 'AFR').
flight('AF1025', 'LPPT', 'LFPG', 1310, 155, 'AFR').

short(Flight) :-
    flight(Flight, _, _, _, Time, _),
    Time < 90.

shorter(Flight1, Flight2, ShorterFlight) :-
    flight(Flight1, _, _, _, Time1, _),
    flight(Flight2, _, _, _, Time2, _),
    (
        Time1 > Time2, ShorterFlight = Flight2;
        Time2 > Time1, ShorterFlight = Flight1
    ), !.

arrivalTime(Flight, ArrivalTime) :-
    flight(Flight, _, _, Departure, Duration, _),
    addMinutes(Departure, Duration, ArrivalTime).

addMinutes(Departure, Duration, ArrivalTime) :-
    DepartureMins is Departure mod 100,
    TotalMins is DepartureMins + Duration,

    ArrivalMinutes is TotalMins mod 60,
    Hours is TotalMins // 60,

    ArrivalHour is ((Departure // 100) + Hours) mod 24,

    ArrivalTime is ArrivalHour*100 + ArrivalMinutes.


countries(Company, ListOfCountries) :-
    findCountries(Company, ListOfCountries, []).

findCountries(Company, [Country | ListOfCountries], Seen) :-
    airport(_, _, Country),
    companyOperatesInCountry(Company, Country),
    \+member(Country, Seen), !,
    findCountries(Company, ListOfCountries, [Country | Seen]).
findCountries(_, [], _) :- !.

companyOperatesInCountry(Company, Country) :-
    flight(_, Origin, Dest, _, _, Company),
    airport(_, Origin, OriginCountry),
    airport(_, Dest, DestCountry),
    (
        OriginCountry = Country;
        DestCountry = Country
    ), !.

pairableFlights :-
    flight(Flight1, _, Airport, _, _, _),
    flight(Flight2, Airport, _, Departure2, _, _),
    Flight1 \= Flight2,
    
    arrivalTime(Flight1, Arrival1),
    TimeDiff is ((Departure2 mod 100) + 60*(Departure2//100))  - ((Arrival1 mod 100) + 60*(Arrival1//100)),
    TimeDiff >= 30, TimeDiff =< 90,

    write(Airport), write(' - '), write(Flight1), write(' \\ '), write(Flight2), nl,
    fail.
pairableFlights :- !.

% in minutes
timeDiff(Time1, Time2, Diff) :-
    Diff is ((Time2 mod 100) + 60*(Time2//100))  - ((Time1 mod 100) + 60*(Time1//100)).


tripDays([_], _, [], 1) :- !.
tripDays([Orig, Dest | Rest], Time, [DepartureTime | FlightTimes], Days) :-
    flight(Flight, OrigAirport, DestAirport, DepartureTime, _, _),
    airport(_, OrigAirport, Orig),
    airport(_, DestAirport, Dest),
    % choose best flight if there are many

    arrivalTime(Flight, ArrivalTime),
    addMinutes(ArrivalTime, 30, NextAvailable),
    tripDays([Dest | Rest], NextAvailable, FlightTimes, RemainingDays),
    (
        Time > ArrivalTime, Days is RemainingDays+1;
        Days is RemainingDays
    ), !.


avgFlightLengthFromAirport(Airport, AvgLength) :-
    findall(Duration, (flight(_, Airport, _, _, Duration, _)), Durations),
    sumlist(Durations, DurSum),
    length(Durations, Ndurations),
    AvgLength is DurSum / Ndurations.


mostInternational(Companies) :-
    findall(Count, (company(Company, _, _, _), supportedCompanyCountries(Company, Count)), Counts),
    sort(Counts, Sorted),
    reverse(Sorted, Reversed),
    [MaxCount | _] = Reversed,
    findall(Company, (company(Company, _, _, _), supportedCompanyCountries(Company, MaxCount)), Companies).

supportedCompanyCountries(Company, Count) :-
    findall(Country, (flight(_, Orig, _, _, _, Company), airport(_, Orig, Country)), OriginCountries),
    findall(Country, (flight(_, _, Dest, _, _, Company), airport(_, Dest, Country)), DestCountries),
    append(OriginCountries, DestCountries, AllCountries),
    list_to_set(AllCountries, NoDups),
    length(NoDups, Count).


dif_max_2(X,Y) :- X < Y, X >= Y-2.

make_pairs([], _, []) :- !.
make_pairs(List, Predicate, [Ele1 - Ele2 | Rest]) :-
    select(Ele1, List, List2),
    select(Ele2, List2, List3),
    G =.. [Predicate, Ele1, Ele2], G,
    make_pairs(List3, Predicate, Rest).