/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

% Used to initialize the game per se

:- consult(logic).

playGame(_) :-
        initialize(L),
        printGameState(L),
        firstPlayer(P),
        play(L, P).
             
% play(GameList, Player).
playGame(L, P) :-
        gameOver(L, P), !,
        write('Over'), nl.

playGame(L, P) :-
        getPlayerMove(L, P, NL),
        printGameState(NL),
        nextPlayer(P, NP),
        playGame(NL, NP).
        
% Player management
firstPlayer(p1).
nextPlayer(p1, p2).
nextPlayer(p2, p1).

% Player move
% getPlayerMove(GameList, Player, NewGameList)
getPlayerMove(L, P, NL).   
                    
% ==============================
%       Game Initialization
% ==============================
% Returns the initial game list, with all the pieces in their initial position

% g - general
% co - coronel
% ca - capitan
% sa - sargeant 
% so - soldier

gameList([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
          [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).

initialize(X) :- gameList(X).

% ==============================
%       Game Over
% ==============================
gameOver(L, P).

% Check player: verifies if any piece of the passed player exists... 