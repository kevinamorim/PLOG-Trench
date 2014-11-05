/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

% Used to initialize the game per se
:- consult(logic).
:- consult(io).

playGame(_) :-
        initialize(L),
        printGameState(L),
        firstPlayer(P),
        playGame(L, P).
             
% play(GameList, Player).
%playGame(L, P) :-
%        gameOver(L, P), !,
%        write('Over'), nl.

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
getPlayerMove(L, Pl, NL) :-
        write('Player: '), write(Pl), nl,
        write('Select Piece (xy): '),
        readPosition([A1,A2]), !,
        write('Select Target (xy): '),
        readPosition([B1,B2]), !,
        convertAlphaToNum(A1, R),
        convertAlphaToNum(A2, C),
        getPiece(L, [R,C], P),
        canMove(L, P, [A1,A2], [B1,B2]), !,
        movePiece(L, [A1,A2], [B1,B2], NL).
                    
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
% gameOver(L, P).

gameOver(_,_). % !REMOVE!

% Check player: verifies if any piece of the passed player exists... 