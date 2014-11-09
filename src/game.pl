/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(logic).
:- consult(io).
:- consult(draw).

% play_game().
play_game :-
        write('Starting game...'), nl,
        first_player(P), write('First player: '), write(P), nl, nl, 
        initialize(L),
        print_board(L),
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
        
        
        \+ player_has_pieces(GameList, p1),
                write('Player 1 has no pieces left.'), nl,
                write('Player 2 wins'), !, nl;
        \+ player_has_pieces(GameList, p2),
                write('Player 2 has no pieces left.'), nl,
                write('Player 1 wins'), !, nl;
        
        
        \+ player_has_moves(GameList, p1, a, i),
                write('Player 1 has no moves left.'), nl,
                write('Player 2 wins'), !, nl;
        \+ player_has_moves(GameList, p2, a, i),
                write('Player 2 has no moves left.'), nl,
                write('Player 1 wins'), !, nl.

% ==============================================
% checks if the player has any piece remaining
% ==============================================
% Prepares checking
player_has_pieces(GameList, Player) :-
        % Get player pieces list
        player_pieces(Player, L),
        player_has_pieces(GameList, L).

% Fail case
player_has_pieces([_|_], []) :- fail.

% For each piece checks if it exists in the gamelist. If any is found, returns yes.
player_has_pieces(GameList, [P|R]) :-
        member_matrix(P,GameList);
        player_has_pieces(GameList, R).
% ==============================================
% ==============================================

% ==============================================
% checks if the player has any pieces width a valid move
% ==============================================

% Gets the next piece for a Player, [X, Y] -> ALPHA MODE
next_piece(GameList, Player, [X, Y], [FX, FY]) :-
        
        convert_alpha_num(Y, Y1),
        Y1 < 9, Y \= p,
                get_next_letter(Y, N, s),
                convert_alpha_num(X, X2),
                convert_alpha_num(N, Y2),
                FX is X, FY is N,
                get_piece(GameList, [X2, Y2], PI),
                check_piece_player(PI, Player);
        
        convert_alpha_num(X, X1),
        X1 < 9, X \= p,
                get_next_letter(X, N, s),
                convert_alpha_num(N, X2),
                convert_alpha_num(Y, Y2),
                FX is N, FY is Y,
                get_piece(GameList, [X2, Y2], PI),
                check_piece_player(PI, Player);

        convert_alpha_num(Y, Y1),
        Y1 < 9, Y \= p,
                get_next_letter(Y, N, s),
                next_piece(GameList, Player, [X, N], [FX, FY]);
        
        convert_alpha_num(X, X1),
        X1 < 9, X \= p,
                get_next_letter(X, N, s),
                next_piece(GameList, Player, [N, Y], [FX, FY]).


piece_has_move(GameList, [PX, PY], [X, Y]) :-
        
        convert_alpha_num(Y, Y1),
        Y1 < 9, Y \= p,
                get_next_letter(Y, N, s),
                can_move(GameList, [PX, PY], [X, N]);
        
        convert_alpha_num(X, X1),
        X1 < 9, X \= p,
                get_next_letter(X, N, s),
                can_move(GameList, [PX, PY], [N, Y]);
        
        convert_alpha_num(Y, Y1),
        Y1 < 9, Y \= p,
                get_next_letter(Y, N, s),
                piece_has_moves(GameList, [PX, PY], [X, N]);
        
        convert_alpha_num(X, X1),
        X1 < 9, X \= p,
                get_next_letter(X, N, s),
                piece_has_moves(GameList, [PX, PY], [N, Y]).

   
% pass alpha everywhere
% player_has_moves(gamelist, player, initX, initY)
player_has_moves(GameList, Player, X, Y) :-
        convert_alpha_num(X, X1),
        convert_alpha_num(Y, Y1),
        X1 < 9, Y1 < 9,
        get_piece(GameList, [X1, Y1], P),
        check_piece_player(P, Player),
        write('checking piece: '), write(P), nl,
        piece_has_moves(GameList, [X, Y], [a, i]).

player_has_moves(GameList, Player, X, Y) :-
        
        convert_alpha_num(Y, Y1),
        Y1 < 9,
                get_next_letter(Y, Y2, s),
                player_has_moves(GameList, Player, X, Y2);
        
        convert_alpha_num(X, X1),
        X1 < 9,
                get_next_letter(X, X2, s),
                player_has_moves(GameList, Player, X2, i);
        fail.

% checks if a piece has a valid movement to it
% piece_has_moves(gamelist, [pieceX, pieceY], [posX, posY]
piece_has_moves(GameList, [X, Y], [A1, A2]) :-
        % Check if is a valid position
        convert_alpha_num(A1, A3),
        convert_alpha_num(A2, A4),
        A3 < 9, A4 < 9,
        % Checks if the movement is possible
        % if the movement is possible, return yes
        can_move(GameList, [X, Y], [A1, A2]).

piece_has_moves(GameList, [X, Y], [A1, A2]) :-
        
        write('next pos...'), nl,
        convert_alpha_num(A2, A3),
        A3 < 9,
                get_next_letter(A2, B1, s),
                piece_has_moves(GameList, [X, Y], [A1, B1]);
        
        convert_alpha_num(A2, A4),
        A4 < 9,
                get_next_letter(A1, B1, s),
                piece_has_moves(GameList, [X, Y], [B1, A2]);
        
        fail.

% ==============================
%       DEBUG 
% ==============================
% Debugging predicates. Not to be used in release.
print_test :- game_list(X), print_board(X).
