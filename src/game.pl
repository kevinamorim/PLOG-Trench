/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(logic).
:- consult(io).

% play_game().
play_game(_) :-
        initialize(L),
        print_board(L),
        first_player(P), !,
        play_game(L, P).
             
% play_game(GameList, Player).
play_game(L, _) :-
        gameOver(L), !, fail.
        
play_game(L, P) :-     
        read_player_move(L, P, NL),
        print_board(NL),
        next_player(P, NP), !,
        play_game(NL, NP).

% Player move
% read_player_move(GameList, Player, NewGameList)
read_player_move(L, Pl, NL) :-
        write('Player: '), write(Pl), nl,
        write('Select Piece (xy): '),
        readPosition([A1,A2]),
        convertAlphaToNum(A1, R),
        convertAlphaToNum(A2, C),
        getPiece(L, [R,C], P),
        checkPiecePlayer(P, Pl), !,
        write('Piece selected: '), get_board_symbol(P, S), write(S), nl,
        write('Select Target (xy): '),
        readPosition([B1,B2]),
        movePiece(L, [A1,A2], [B1,B2], NL), !;
        %else : happens when the player inserts coordinates of a piece that it's not his
        read_player_move(L, Pl, NL).
                    
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