% ===========================================
%       "Includes" <-- haha C rocks
% ===========================================
:- consult(list_stuff).
:- consult(converter).
:- consult(piece).
:- consult(player).

% ===========================================
%       Pieces getter & setter
% ===========================================
% Predicate that effectively moves a piece from one place to another.
% Here all movements and verifications are made. 
% movePiece(GameList, [From], [To], NewGameList).
%       Coordinates in alpha
move_piece(L, [X1, Y1], [X2, Y2], NL) :-
        get_piece(L, [X1, Y1], P),
        set_piece(L, e, [X1, Y1], L1),
        set_piece(L1, P, [X2, Y2], NL).

% ===========================================
%       Pieces getter & setter
% ===========================================
% getPiece(GameList, [Row, Column], Piece).
%       Coordinates in alpha
get_piece(L, [R, C], P) :-  
        convert_alpha_num(R, R1),
        convert_alpha_num(C, C1),
        convert_to_grid_pos(R1, C1, Row, Col),
        select_elem(Row, Col, L, P).

% setPiece(GameList, Piece, [Pos], NewGameList)
%       Coordinates in alpha
set_piece(L, P, [R, C], NL) :-
        convert_alpha_num(R, R1),
        convert_alpha_num(C, C1),
        convert_to_grid_pos(R1, C1, Row, Col),
        nth1(Row, L, X),
        replace(X, Col, P, Res),
        replace(L, Row, Res, NL).

% ===========================================
%       Pieces movement
% ===========================================
% Predicate that checks if a given piece can move from a pos to another.

% canMove(GameList, [FromX,FromY], [ToX,ToY], Player)
%       Coordinates in alpha
can_move(L, [R1, C1], [R2, C2], Player) :-
        R1 == R2, C1 == C2,
        %write('>>> fail: '), write([R1,C1,R2,C2]), nl,
        !, fail;
        
        % else
        get_piece(L, [R1, C1], PI),
        %write('Piece: '), write(PI), nl,
        get_distance([R1, C1], [R2, C2], DIST),
        %write('Distance: '), write(DIST), nl,
        max_distance_for(PI, MAX),
        %write('Max: '), write(MAX), nl,
        DIST < (MAX + 1),       % Distance verification
        get_direction([R1, C1], [R2, C2], DIR),
        %write('Direction: '), write(DIR), nl,
        get_allowed_dir_for(PI, DIR),
        %write('Ei men passou tudo'), nl,
        !,
        check_road(L, [R1,C1], [R1,C1], [R2,C2], Player, 0).


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
        R1 \== R2, C1 \== C2, convert_alpha_num(R1, X), convert_alpha_num(R2, Y), calculate_distance(X, Y, D).

% Calculates the distance between two coordinates.
calculate_distance(SRC, DST, D) :-
        SRC \== DST, D is abs(SRC - DST).
% ===========================================
% ===========================================

% ===========================================
% DIRECTION
% ===========================================
% get_direction([SrcX, SrcY], [DestX, DestY], Direction)) : Coordinates in alpha mode
% 'd' -> Diagonal
get_direction([R1, C1], [R2, C2], D) :-
        R1 == R2, C1 \== C2, D = d; 
        R1 \== R2, C1 == C2, D = d;
        R1 \== R2, C1 \== C2, get_perpendicular_direction([R1, C1], [R2, C2], D).

get_perpendicular_direction([R1, C1], [R2, C2], D) :-  
        
        % South Movement
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2),      
        X1 < X2, Y1 < Y2, D = south;
        
        % North Movement
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2),  
        X1 > X2, Y1 > Y2, D = north;
        
        % East Movement
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2), 
        X1 > X2, Y1 < Y2, D = east;
        
        % West Movement
        convert_alpha_num(R1, X1),
        convert_alpha_num(C1, Y1),   
        convert_alpha_num(R2, X2),
        convert_alpha_num(C2, Y2), 
        X1 < X2, Y1 > Y2, D = west. 

% ===========================================
% VERIFICATION OF TRAJECTS
% ===========================================
%       Coordinates in alpha
check_road(L, [P1,P2], [R1,C1], [R2, C2], Player, Count) :-
        
        % Finish it
        R1 == R2, C1 == C2,
                can_capture(L, [P1,P2], [R2,C2], Player);
        
        % Finish it, but softly
        /*
        R1 == R2, C1 == C2, 
                % if no piece is there
                        is_valid_house(L, [R1,C1], Count);
        */
        
        % southeast                                                         
        R1 == R2, C1 \= C2,
                convert_alpha_num(C1, Y1),
                convert_alpha_num(C2, Y2),
                Y2 > Y1,
                        is_valid_house(L, [R1,C1], Count),
                        %write('> southeast'), nl,
                        get_next_letter(C1, TC, s),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [R1, TC], [R2, C2], Player, Count1);
        
        % northwest
        R1 == R2, C1 \= C2,
                convert_alpha_num(C1, Y1),
                convert_alpha_num(C2, Y2),
                Y2 < Y1,
                        is_valid_house(L, [R1,C1], Count),
                        %write('> northwest'), nl,
                        get_next_letter(C1, TC, n),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [R1, TC], [R2, C2], Player, Count1);
        
        % southwest
        C1 == C2,  R1 \= R2,
                convert_alpha_num(R1, X1),
                convert_alpha_num(R2, X2),
                X2 > X1,
                        is_valid_house(L, [R1,C1], Count),
                        %write('> southwest'), nl,
                        get_next_letter(R1, TR, s),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [TR, C1], [R2, C2], Player, Count1);
        
        % northeast
        C1 == C2, R1 \= R2,
                convert_alpha_num(R1, X1),
                convert_alpha_num(R2, X2),
                X2 < X1,
                        is_valid_house(L, [R1,C1], Count),
                        %write('> northeast'), nl,
                        get_next_letter(R1, TR, n),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [TR, C1], [R2, C2], Player, Count1);
        
        % south
        R1 \= R2, C1 \= C2,
                convert_alpha_num(R1, X1),
                convert_alpha_num(C1, Y1),
                convert_alpha_num(R2, X2),
                convert_alpha_num(C2, Y2),
                IncX is X2 - X1, IncY is Y2 - Y1, 
                IncX == IncY, X2 > X1,
                        is_valid_house(L, [R1,C1], Count),
                        get_next_letter(R1, TR, s),
                        get_next_letter(C1, TC, s),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [TR, TC], [R2, C2], Player, Count1);
        
        % should be north lol
        R1 \= R2, C1 \= C2,
                convert_alpha_num(R1, X1),
                convert_alpha_num(C1, Y1),
                convert_alpha_num(R2, X2),
                convert_alpha_num(C2, Y2),
                IncX is X2 - X1, IncY is Y2 - Y1, 
                IncX == IncY, X2 < X1,
                        is_valid_house(L, [R1,C1], Count),
                        get_next_letter(R1, TR, n),
                        get_next_letter(C1, TC, n),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [TR, TC], [R2, C2], Player, Count1);
        
        % east
        R1 \= R2, C1 \= C2,
                convert_alpha_num(R1, X1),
                convert_alpha_num(C1, Y1),
                convert_alpha_num(R2, X2),
                convert_alpha_num(C2, Y2),
                IncX is X1 - X2, IncY is Y2 - Y1, 
                IncX == IncY, X1 > X2,
                        is_valid_house(L, [R1,C1], Count),
                        get_next_letter(R1, TR, n),
                        get_next_letter(C1, TC, s),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [TR, TC], [R2, C2], Player, Count1);
        
        % west
        R1 \= R2, C1 \= C2,
                convert_alpha_num(R1, X1),
                convert_alpha_num(C1, Y1),
                convert_alpha_num(R2, X2),
                convert_alpha_num(C2, Y2),
                IncX is X1 - X2, IncY is Y2 - Y1, 
                IncX == IncY, X1 < X2,
                        is_valid_house(L, [R1,C1], Count),
                        get_next_letter(R1, TR, s),
                        get_next_letter(C1, TC, n),
                        Count1 is Count + 1,
                        check_road(L, [P1,P2], [TR, TC], [R2, C2], Player, Count1);
        
        !, fail.
      
% ===========================================
%   Checking stuff
% ===========================================
is_valid_house([_|_], [_,_], 0) :- !.
        
is_valid_house(L, [R,C], Count) :-
        Count > 0,
               is_empty_piece(L, [R,C]).

can_capture(L, [P1,P2], [T1,T2], Player) :-
        
        is_empty_piece(L, [T1,T2]);
        
        is_trench([T1,T2]),
                get_piece(L, [T1, T2], Piece),
                \+ check_piece_player(Piece, Player),
                convert_alpha_num(P1,X),
                convert_alpha_num(P2,Y),
                convert_to_grid_pos(X, Y, Row, _),
                %write('Row: '), write(Row), nl,
                in_enemy_turf(Player, Row);
        
        get_piece(L, [P1, P2], Piece),
        \+ check_piece_player(Piece, Player).

%
is_empty_piece(L, [R,C]) :-
        get_piece(L, [R,C], PI),
        PI == e.

% ===========================================
% Le Trench
% ===========================================
is_trench([R, C]) :-
        convert_alpha_num(R,X),
        convert_alpha_num(C,Y),
        convert_to_grid_pos(X, Y, Row, _),
        Row == 8.
% ===========================================
% ===========================================