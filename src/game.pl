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
        game_over(L), !.
        
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

game_list([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
         [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).

initialize(X) :- game_list(X).

% ==============================
%       Game Over
% ==============================
%  A game is considered over in the following situations:
%       - A players as lost all of its pieces;
%       - None of the pieces in the board has a valid move;
%       - No pieces has been 'consumed' in 30 moves.
game_over(GameList) :-
        \+ player_has_pieces(GameList, p1), write('Player 2 wins'), nl;
        \+ player_has_pieces(GameList, p2), write('Player 1 wins'), nl;
        \+ player_has_moves(GameList, p1), write('Player 2 wins'), nl;
        \+ player_has_moves(GameList, p2), write('Player 1 wins'), nl.

% checks if the player has any piece remaining
player_has_pieces(GameList, Player) :-
        playerPieces(Player, L),
        player_has_pieces(GameList, L).

player_has_pieces([_|_], []) :- fail.

player_has_pieces(GameList, [P|R]) :-
        member_matrix(P,GameList);
        player_has_pieces(GameList, R).

% checks if the player has any piece with a valid move
player_has_moves(GameList, Player) :-
        player_has_moves(GameList, Player, 1, 1).

player_has_moves(GameList, Player, X, Y) :-
        %write('player_has_moves(GameList,'), write(Player), write(','),
        %write(X), write(','), write(Y), write(')'), nl,
        X < 9, !,
                X1 is X + 1,
                player_has_moves(GameList, Player, X1, Y),
                getPiece(GameList, [X,Y], P),
                checkPiecePlayer(P, Player), !,
                piece_has_moves(GameList, [X,Y], [1,1]);
        Y < 9, !,
                Y1 is Y + 1,
                player_has_moves(GameList, Player, 1, Y1),
                getPiece(GameList, [X,Y], P),
                checkPiecePlayer(P, Player), !,
                piece_has_moves(GameList, [X,Y], [1,1]).

player_has_moves([_|_], _, _, _).

piece_has_moves([_|_], [X|Y], [X|Y]) :- fail.

piece_has_moves(GameList, [X,Y], [A1,A2]) :-
        A1 < 9, !,
                B1 is A1 + 1,
                piece_has_moves(GameList, [X,Y], [B1,A2]),
                canMove(GameList, [X,Y], [A1,A2]);
        A2 < 9, !,
                B2 is A2 + 1,
                piece_has_moves(GameList, [X,Y], [1,B2]);
                canMove(GameList, [X,Y], [A1,A2]).

piece_has_moves([_|_], [_], [_]).

               
