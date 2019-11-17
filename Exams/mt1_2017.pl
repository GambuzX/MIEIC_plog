:- use_module(library(lists)).
:- dynamic played/4.

%player(Name, Username, Age)
player('Danny', 'Best Player Ever', 27).
player('Annie', 'Worst Player Ever', 24).
player('Harry', 'A-Star Player', 26).
player('Manny', 'The Player', 14).
player('Johnny', 'A Player', 16).

%game(Name, Categories, MinAge)
game('5 ATG', [action, adventure, open-world, multiplayer], 18).
game('Carrier Shift: Game Over', [action, fps, multiplayer, shooter], 16).
game('Duas Botas', [action, free, strategy, moba], 12).

%played(Player, Game, HoursPlayed, PercentUnlocked)
played('Best Player Ever', '5 ATG', 3, 83).
played('Worst Player Ever', '5 ATG', 52, 9).
played('The Player', 'Carrier Shift: Game Over', 44, 22).
played('A Player', 'Carrier Shift: Game Over', 48, 24).
played('A-Star Player', 'Duas Botas', 37, 16).
played('Best Player Ever', 'Duas Botas', 33, 22).


achievedALot(Player) :-
    player(Player, _, _, Percentage),
    Percentage >= 80.

isAgeAppropriate(Name, Game) :-
    player(Name, _, PlayerAge),
    game(Game, _, GameAgeLimit),
    PlayerAge >= GameAgeLimit.

timePlayingGames(_, [], [], 0).
timePlayingGames(Player, [CurrGame | Rest], [TimePlayed | ListOfTimes], SumTimes) :-
    (
        played(Player, CurrGame, TimePlayed, _);
        TimePlayed = 0
    ), !,
    timePlayingGames(Player, Rest, ListOfTimes, RestTimes),
    SumTimes is RestTimes + TimePlayed.


findCategory(Cat, [Cat | _]) :- !.
findCategory(Cat, [_ | Rest]) :-
    findCategory(Cat, Rest).

listGamesOfCategory(Cat) :-
    game(GameName, Categories, MinAge),
    findCategory(Cat, Categories),
    write(GameName), write(' ('), write(MinAge), put_char(')'), nl,
    fail.
listGamesOfCategory(_) :- !.



updatePlayer(Player, Game, Hours, Percentage) :-
    retract(played(Player, Game, PrevHours, PrevPercentage)), !,
    NewHours is PrevHours + Hours,
    NewPercentage is PrevPercentage + Percentage,
    assert(played(Player, Game, NewHours, NewPercentage)).

updatePlayer(Player, Game, Hours, Percentage) :-
    assert(played(Player, Game, Hours, Percentage)).


fewHours(Player, Games) :-
    fewHoursAux(Player, Games, []).

fewHoursAux(Player, [NewGame | Games], Seen) :-
    played(Player, NewGame, Time, _),
    Time < 10,
    \+member(NewGame, Seen), !,
    fewHoursAux(Player, Games, [NewGame | Seen]).

fewHoursAux(_, [], _) :- !.


ageRange(MinAge, MaxAge, Players) :-
    findall(Player, (player(Player, _, Age), Age >= MinAge, Age =< MaxAge), Players).
    
averageAge(Game, AverageAge) :-
    findall(Age, (played(Player, Game, _, _), player(_, Player, Age)), Ages),
    sumlist(Ages, AgesTotal),
    length(Ages, NAges),
    AverageAge is AgesTotal/NAges.



mostEffectivePlayers(Game, [BestPlayer | OtherBestPlayers]) :-
    findall(Ratio-Player, (played(Player, Game, Time, Completion), Ratio is Completion/Time), GamePlayers),
    sort(GamePlayers, SortedGP),
    reverse(SortedGP, ReversedGP),
    [MaxRatio - BestPlayer | RestPlayers] = ReversedGP,
    findOtherBestPlayers(RestPlayers, MaxRatio, OtherBestPlayers).

findOtherBestPlayers([], _, []) :- !.

findOtherBestPlayers([TargetRatio - User | RestPlayers], TargetRatio, [User | BestPlayers]) :-
    findOtherBestPlayers(RestPlayers, TargetRatio, BestPlayers), !.

findOtherBestPlayers([_ | RestPlayers], TargetRatio, BestPlayers) :-
    findOtherBestPlayers(RestPlayers, TargetRatio, BestPlayers), !.

