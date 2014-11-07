/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(logic).
:- consult(io).
:- consult(draw).

% play_game().
play_game :-
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
        convert_alpha_num(A1, R),
        convert_alpha_num(A2, C),
        get_piece(L, [R,C], P),
        check_piece_player(P, Pl), !,
        write('Piece selected: '), get_board_symbol(P, S), write(S), nl,
        write('Select Target (xy): '),
        readPosition([B1,B2]),
        move_piece(L, [A1,A2], [B1,B2], NL), !;
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
        \+ player_has_pieces(GameList, p1), write('Player 2 wins'), !, nl;
        \+ player_has_pieces(GameList, p2), write('Player 1 wins'), !, nl;
        player_has_moves(GameList, p1, 1, 1), write('Player 2 wins'), !, nl;
        player_has_moves(GameList, p2, 1, 1), write('Player 1 wins'), !, nl.

% checks if the player has any piece remaining
player_has_pieces(GameList, Player) :-
        player_pieces(Player, L),
        player_has_pieces(GameList, L).

player_has_pieces([_|_], []) :- fail.

player_has_pieces(GameList, [P|R]) :-
        member_matrix(P,GameList);
        player_has_pieces(GameList, R).

player_has_moves(GameList, Player, X, Y) :-
        X < 9, Y < 9,
        get_piece(GameList, [X,Y], P),
        check_piece_player(P, Player),
        write('> Piece: '), write(P), nl,
        piece_has_moves(GameList, [X,Y], [1,1]).

player_has_moves(GameList, Player, X, Y) :-
        X < 9, !,
                X1 is X + 1,
                player_has_moves(GameList, Player, X1, Y);
        Y < 9, !,
                Y1 is Y + 1,
                player_has_moves(GameList, Player, 1, Y1);
        fail.

piece_has_moves([_|_], [X,Y], [X,Y]) :- fail.

piece_has_moves(GameList, [X,Y], [A1,A2]) :-
        A1 < 9, !,
                B1 is A1 + 1,
                piece_has_moves(GameList, [X,Y], [B1,A2]),
                can_move(GameList, [X,Y], [A1,A2]);
        A2 < 9, !,
                B2 is A2 + 1,
                piece_has_moves(GameList, [X,Y], [1,B2]);
                can_move(GameList, [X,Y], [A1,A2]).

piece_has_moves([_|_], [_], [_]).

% ==============================
%       DEBUG 
% ==============================
% Debugging predicates. Not to be used in release.
print_test :- game_list(X), print_board(X).
