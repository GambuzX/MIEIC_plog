% X existe_em L
:- op(200,xfx,existe_em).

X existe_em [X|_].

X existe_em [_|L]:-
    X existe_em L. 

% concatena L1 e L2 da L3
:- op(400, fx, concatena).
:- op(300, xfx, da).
:- op(200, xfx, e).

concatena [] e L da L.
concatena [H | L1] e L2 da [H | L3] :-
    concatena L1 e L2 da L3.

% apaga Elemento a Lista da NovaLista
:- op(400, fx, apaga).
:- op(200, xfx, a).

apaga _ a [] da [].
apaga X a [X | Rest] da Res :-
    apaga X a Rest da Res.
apaga X a [H | Rest] da [H | Res] :-
    X \= H,
    apaga X a Rest da Res.
