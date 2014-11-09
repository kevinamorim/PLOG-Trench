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
        %write('can move: '), write(R1), write(C1), write(R2), write(C2), nl,
        %R1 == R2, C1 == C2;
        
        convert_alpha_num(R1, A1),
        convert_alpha_num(C1, A2),
        get_piece(L, [A1, A2], PI),
        %write('Piece: '), write(PI), nl,
        get_distance([R1, C1], [R2, C2], DIST),
        %write('Distance: '), write(DIST), nl,
        max_distance_for(PI, MAX),
        %write('Max: '), write(MAX), nl,
        DIST < (MAX + 1),       % Distance verification
        get_direction(L, [R1, C1], [R2, C2], DIR),
        %write('Direction: '), write(DIR), nl,
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
        R1 \== R2, C1 \== C2, convert_alpha_num(R1, X), convert_alpha_num(R2, Y),calculate_distance(X, Y, D).

% Calculates the distance between two coordinates.
calculate_distance(SRC, DST, D) :-
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
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2),      
        get_piece(L, [X1, Y1], PI),
        check_piece_player(PI, P),
        P == p1,
        X1 < X2, Y1 < Y2, D = f;
        
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2),   
        get_piece(L, [X1, Y1], PI),
        check_piece_player(PI, P),
        P == p2,
        X1 < X2, Y1 < Y2, D = b;
        
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2),  
        get_piece(L, [X1, Y1], PI),
        check_piece_player(PI, P),
        P == p1,
        X1 > X2, Y1 > Y2, D = b;
        
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2), 
        get_piece(L, [X1, Y1], PI),
        check_piece_player(PI, P),
        P == p2,
        X1 > X2, Y1 > Y2, D = f;
        
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2), 
        X1 > X2, Y1 < Y2, D = r;
         
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2), 
        X1 < X2, Y1 > Y2, D = l. 

% ===========================================
% ===========================================
% ===========================================
% VERIFICATION OF TRAJECTS
% ===========================================
% -----> Positions in alpha

check_road(L, [R1, C1], [R2, C2]) :-
        R1 == R2,
        C1 \= C2,
        convert_to_grid_pos(R1, C1, X1, _),
        convert_to_grid_pos(R2, C2, X2, _),
        X1 > X2,
        check_road_diagonal(L, [R1, C1], [R2, C2], nw);
        
        R1 == R2,
        C1 \= C2,
        convert_to_grid_pos(R1, C1, X1, _),
        convert_to_grid_pos(R2, C2, X2, _),
        X1 < X2,
        check_road_diagonal(L, [R1, C1], [R2, C2], se);
        
        C1 == C2,
        R1 \= R2,
        convert_to_grid_pos(R1, C1, X1, _),
        convert_to_grid_pos(R2, C2, X2, _),
        X1 > X2,
        check_road_diagonal(L, [R1, C1], [R2, C2], ne);
        
        C1 == C2,
        R1 \= R2,
        convert_to_grid_pos(R1, C1, X1, _),
        convert_to_grid_pos(R2, C2, X2, _),
        X1 < X2,
        check_road_diagonal(L, [R1, C1], [R2, C2], sw);
        
        R1 \= R2,
        C1 \= C2,
        char_code(R1, X1), char_code(R2, X2), char_code(C1, Y1), char_code(C2, Y2),
        X1 < X2, Y1 < Y2, 
        check_road_vertical(L, [R1, C1], [R2, C2], s);
        
        R1 \= R2,
        C1 \= C2,
        char_code(R1, X1), char_code(R2, X2), char_code(C1, Y1), char_code(C2, Y2),
        X1 > X2, Y1 > Y2, 
        check_road_vertical(L, [R1, C1], [R2, C2], s);
        
        R1 \= R2,
        C1 \= C2,
        char_code(R1, X1), char_code(R2, X2), char_code(C1, Y1), char_code(C2, Y2),
        X1 > X2, Y1 < Y2,
        check_road_horizontal(L, [R1, C1], [R2, C2], e);
        
        R1 \= R2,
        C1 \= C2,
        char_code(R1, X1), char_code(R2, X2), char_code(C1, Y1), char_code(C2, Y2),
        X1 < X2, Y1 > Y2,
        check_road_horizontal(L, [R1, C1], [R2, C2], w);
        
        R1 == R2, C1 == C2.

% check_road_vertical([posSrc], [posDest], dir(n/s))
% example: check_road_vertical([a, i], [p, h], s)
% Base case
% Works for north and south movements... 
% -> Tested
% Base
check_road_vertical(_, [R1, C1], [R2, C2], _) :-
        R1 == R2, C1 == C2. 

check_road_vertical(L, [R1, C1], [R2, C2], DIR) :-
        R1 \= R2, C1 \= C2, % The src and dest cant be equal
        get_next_letter(R1, TR, DIR),
        get_next_letter(C1, TC, DIR),
        convert_alpha_num(TR, X),
        convert_alpha_num(TC, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_vertical(L, [TR, TC], [R2, C2], DIR).   

% check_road_horizontal([posSrc], [posDest], dir(e/w))
% -> Tested: more testing is needed.
% Base
check_road_horizontal(_, [R1, C1], [R2, C2], _) :- 
        R1 == R2, C1 == C2.

check_road_horizontal(L, [R1, C1], [R2, C2], DIR) :-
        % moving to the right
        R1 \= R2, C1 \= C2, 
        DIR == e, 
        get_next_letter(R1, TR, n),
        get_next_letter(C1, TC, s),
        convert_alpha_num(TR, X),
        convert_alpha_num(TC, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_horizontal(L, [TR, TC], [R2, C2], DIR);
        
        % moving to the left
        R1 \= R2, C1 \= C2, 
        DIR == w,
        get_next_letter(R1, TR, s),
        get_next_letter(C1, TC, n),
        convert_alpha_num(TR, X),
        convert_alpha_num(TC, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_horizontal(L, [TR, TC], [R2, C2], DIR).

% check_road_diagonal([posSrc], [posDest], dir(nw, ne, sw, se))
% -> Tested: but needs more
check_road_diagonal(_, [R1, C1], [R2, C2], _) :- 
        R1 == R2, C1 == C2.

check_road_diagonal(L, [R1, C1], [R2, C2], DIR) :-
        % northeast
        DIR == ne,
        R1 \= R2,
        get_next_letter(R1, TR, n),
        convert_alpha_num(TR, X),
        convert_alpha_num(C1, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_diagonal(L, [TR, C1], [R2, C2], DIR);

        % northwest
        DIR == nw,
        C1 \= C2,
        get_next_letter(C1, TC, n),
        convert_alpha_num(R1, X),
        convert_alpha_num(TC, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_diagonal(L, [R1, TC], [R2, C2], DIR);

        % southeast
        DIR == se,
        C1 \= C2,
        get_next_letter(C1, TC, s),
        convert_alpha_num(R1, X),
        convert_alpha_num(TC, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_diagonal(L, [R1, TC], [R2, C2], DIR);
        
        % southwest
        DIR == sw,
        R1 \= R2,
        get_next_letter(R1, TR, s),
        convert_alpha_num(TR, X),
        convert_alpha_num(C1, Y),
        get_piece(L, [X, Y], PI),
        PI == e,
        check_road_diagonal(L, [TR, C1], [R2, C2], DIR).

% ===========================================
% ===========================================