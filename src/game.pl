/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

% Used to initialize the game per se
:- consult(logic).
:- consult(io).

playGame(_) :-
        initialize(L),
        printGameState(L),
        firstPlayer(P), !,
        playGame(L, P).
             
% play(GameList, Player).
playGame(L, _) :-
        gameOver(L), !, fail.
        
playGame(L, P) :-     
        getPlayerMove(L, P, NL),
        printGameState(NL),
        nextPlayer(P, NP), !,
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
        convertAlphaToNum(A1, R),
        convertAlphaToNum(A2, C),
        getPiece(L, [R,C], P),
        write('Piece selected: '), getSymbol(P, S), write(S), nl,
        write('Select Target (xy): '),
        readPosition([B1,B2]), !,
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

/*
gameList([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
         [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).
*/

gameList([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
         [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, e, e, e, e], [e, e, e, e, e, e], [e, e, e, e, e], [e, e, e, e],
         [e, e, e], [e, e], [g2]]).


initialize(X) :- gameList(X).

% ==============================
%       Game Over
% ==============================
%  A game is considered over in the following situations:
%       - A players as lost all of its pieces;
%       - None of the pieces in the board has a valid move;
%       - No pieces has been 'consumed' in 30 moves.
gameOver(GameList) :-
        \+ playerHasPiecesLeft(GameList, p1), write('Player 2 wins'), nl;
        \+ playerHasPiecesLeft(GameList, p2), write('Player 1 wins'), nl.

playerHasPiecesLeft(GameList, Player) :-
        playerPieces(Player, L),
        playerHasPieces(GameList, L).

playerHasPieces([_|_], []) :- fail.

playerHasPieces(GameList, [P|R]) :-
        member_matrix(P,GameList);
        playerHasPieces(GameList, R).

% Check player: verifies if any piece of the passed player exists... 



printTest(_) :- initialize(X), printGameState(X).
