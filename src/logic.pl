% ===========================================
%       Includes
% ===========================================
:- consult(list_stuff).
:- consult(converter).
:- consult(piece).
:- consult(player).
% ===========================================
% ===========================================

% ===========================================
%       Pieces getter & setter
% ===========================================
% Predicate that effectively moves a piece from one place to another.
% Here all movements and verifications are made. 

% movePiece(GameList, [From], [To], NewGameList).

move_piece(L, [X1, Y1], [X2, Y2], NL) :-
        convert_alpha_num(X1, R1),
        convert_alpha_num(Y1, C1),
        convert_alpha_num(X2, R2),
        convert_alpha_num(Y2, C2),
        get_piece(L, [R1, C1], P),
        can_move(L, [X1, Y1], [X2, Y2]), % Verifies if the move can be done
        set_piece(L, e, [R1, C1], L1),
        set_piece(L1, P, [R2, C2], NL).

% ===========================================
%       Pieces getter & setter
% ===========================================
% getPiece(GameList, [Row, Column], Piece).
%       Row and Columns must be in:         [1, 2, 3, 4, 5, 6, 7, 8]
get_piece(L, [R,C], P) :-
        convert_to_grid_pos(R, C, Row, Col),
        select_elem(Row, Col, L, P).


% setPiece(GameList, Piece, [Pos], NewGameList)
set_piece(L, P, [R, C], NL) :-
        convert_to_grid_pos(R, C, Row, Col),
        nth1(Row, L, X),
        replace(X, Col, P, Res),
        replace(L, Row, Res, NL).
% ===========================================
% ===========================================

% ===========================================
%       Pieces movement
% ===========================================
% Predicate that checks if a given piece can move from a pos to another.

% canMove(GameList, [From], [To])
can_move(L, [R1, C1], [R2, C2]) :- 
        get_piece(L, [R1, C1], PI),
        get_distance([R1, C1], [R2, C2], DIST),
        max_distance_for(PI, MAX),
        DIST < (MAX + 1),       % Distance verification
        get_direction(L, [R1, C1], [R2, C2], DIR),
        get_allowed_dir_for(PI, DIR).

% ===========================================
% DISTANCE
% ===========================================
% getDistance([SrcRow, SrcCol], [DestRow, DestCol], distance)
% Checks first if the movement is diagonal or perpendicular. 
% If only one of the coordinates changes: diagonal
% If both coordinates changes: perpendicular
% We use a generic method for calculating the distance, passing to it the two coordinates that will 
%   make the distance.
get_distance([R1, C1], [R2, C2], D) :- 
        R1 == R2, C1 \== C2, convert_alpha_num(C1, X), convert_alpha_num(C2, Y), calculate_distance(X, Y, D).

get_distance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 == C2, convert_alpha_num(R1, X), convert_alpha_num(R2, Y), calculate_distance(X, Y, D).

get_distance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 \== C2, convert_alpha_num(R1, X), convert_alpha_num(R2, Y),calculate_distancee(X, Y, D).

% Calculates the distance between two coordinates.
calculate_distancee(SRC, DST, D) :-
        SRC \== DST, D is abs(SRC - DST).
% ===========================================
% ===========================================

% ===========================================
% DIRECTION
% ===========================================
% 'f' -> Front
% 'b' -> Back
% 'l' -> Left
% 'r' -> Right
% 'd' -> Diagonal
get_direction(L, [R1, C1], [R2, C2], D) :-
        R1 == R2, C1 \== C2, D = d; 
        R1 \== R2, C1 == C2, D = d;
        R1 \== R2, C1 \== C2, get_perpendicular_direction(L, [R1, C1], [R2, C2], D).

get_perpendicular_direction(L, [R1, C1], [R2, C2], D) :-
        get_piece(L, [R1, C1], PI),
        check_piece_player(PI, P),
        P == p1,
        R1 < R2, C1 < C2, D = f;
        
        get_piece(L, [R1, C1], PI),
        check_piece_player(PI, P),
        P == p2,
        R1 < R2, C1 < C2, D = b;
        
        get_piece(L, [R1, C1], PI),
        check_piece_player(PI, P),
        P == p1,
        R1 > R2, C1 > C2, D = b;
        
        get_piece(L, [R1, C1], PI),
        check_piece_player(PI, P),
        P == p2,
        R1 > R2, C1 > C2, D = f;
        
        R1 > R2, C1 < C2, D = r;
         
        R1 < R2, C1 > C2, D = l. 

% ===========================================
% ===========================================

% ??????????????????????????????
% ||||||||||Sandbox|||||||||||||
% ??????????????????????????????

verify_traject(L, [R1, C1], [R2, C2]) :-
        get_direction(L, [R1, C1], [R2, C2], DIR),
        convert_alpha_num(R1, A1),
        convert_alpha_num(R2, A2),
        get_piece(L, [A1, A2], PI),
        check_piece_player(PI, P),
        P == p1,
        DIR == f,
        check_road_front_p1(L, [R1, R2], A2 - A1).

check_road_front_p1(_, _, 0) :- !.
check_road_front_p1(L, [X1, X2], T) :- 
        convert_alpha_num(X1, A0), 
        convert_alpha_num(X2, A2), 
        A1 = A0 + T, 
        get_piece(L, [A1, A2], PI), 
        PI == e,
        Y is T - 1,
        check_road_front_p1(L, [X1, X2], Y).

