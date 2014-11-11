:- consult(logic).
:- consult(draw).

game_list([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
         [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).

test :- game_list(X), cpu_moves(X, p2, [1,1], Moves), write(Moves).

test2 :- game_list(X), write(X).

cpu_moves(GameList, Player, [X, Y], Moves) :-
        Y == 9, Moves = [];

        X < 9,
                convert_alpha_num(R,X),
                convert_alpha_num(C,Y),
                get_piece(GameList, [R, C], P),
                check_piece_player(P, Player), !,
                %write('> players piece: '), get_board_symbol(P, S), write(S), write(' at: '), write([X,Y]), nl, 
                cpu_piece_has_moves(GameList, [X, Y], [1, 1], Player, PieceMoves), !,
                X1 is X + 1,
                cpu_moves(GameList, Player, [X1, Y], NewMoves),
                append([[X,Y]],[PieceMoves], NewPieceMoves),
                write(NewPieceMoves), nl,
                append([NewPieceMoves], [NewMoves], Moves);
        
        X < 9, !,
                X1 is X + 1,
                cpu_moves(GameList, Player, [X1, Y], Moves);
        
        Y < 9, !,
                Y1 is Y + 1,
                cpu_moves(GameList, Player, [1, Y1], Moves).

%
cpu_piece_has_moves(GameList, [R,C], [TR,TC], Player, PieceMoves) :-
        
        TC == 9, PieceMoves = [];
        
        TR < 9,
                NR is TR + 1,
                %write('>> call : '), write([R,C]), write([TR,TC]), nl,
                %write('New: '), write(NewPieceMoves), nl,
                %write('>>> try move: '), write([A1, A2]), write([B1, B2]), nl,
                convert_alpha_point([A1,A2], [R,C]),
                convert_alpha_point([B1,B2], [TR,TC]),
                can_move(GameList, [A1, A2], [B1, B2], Player), !,
                %write('>>> can move'), nl,
                cpu_piece_has_moves(GameList, [R,C], [NR,TC], Player, NewPieceMoves), !,
                append([[B1,B2]], NewPieceMoves, PieceMoves);
                %write(PieceMoves), nl;
        
        % can_move failed
        TR < 9, !,
                NR is TR + 1,
                cpu_piece_has_moves(GameList, [R,C], [NR,TC], Player, NewPieceMoves),
                append([], NewPieceMoves, PieceMoves);
                %write(PieceMoves), nl;
        
        TC < 9, !,
                NC is TC + 1,
                cpu_piece_has_moves(GameList, [R,C], [1,NC], Player, PieceMoves).

        
        