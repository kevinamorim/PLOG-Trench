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
move_piece(L, [P1, P2], [T1, T2], NL) :-
        get_piece(L, [P1, P2], P),
        get_direction([P1, P2], [T1, T2], D),
        %write('move_piece -> capture_all()'), nl,
        R = P1, C = P2,
        capture_all(L, [P1, P2], [R, C], [T1, T2], D, L1),
        set_piece(L1, P, [T1, T2], NL).

capture_all(L, [P1, P2], [R,C], [T1, T2], D, NL) :-
        
        R == T1, C == T2, NL = L;
        
        set_piece(L, e, [R,C], L1),
        inc([R,C], [TR,TC], D),
        capture_all(L1, [P1, P2], [TR, TC], [T1, T2], D, NL).
        
% ===========================================
%       Pieces getter & setter
% ===========================================
% getPiece(GameList, [Row, Column], Piece).
%       Coordinates in alpha
get_piece(L, [R, C], P) :-
        convert_alpha_point([R, C], [R1, C1]),  
        convert_to_grid_pos(R1, C1, Row, Col),
        select_elem(Row, Col, L, P).

% setPiece(GameList, Piece, [Pos], NewGameList)
%       Coordinates in alpha
set_piece(L, P, [R, C], NL) :-
        convert_alpha_point([R, C], [R1, C1]),  
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
        R1 == R2, C1 \== C2, convert_alpha_point([C1, C2], [X, Y]),  calculate_distance(X, Y, D).

get_distance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 == C2, convert_alpha_point([R1, R2], [X, Y]), calculate_distance(X, Y, D).

get_distance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 \== C2,convert_alpha_point([R1, R2], [X, Y]), calculate_distance(X, Y, D).

% Calculates the distance between two coordinates.
calculate_distance(SRC, DST, D) :-
        SRC \== DST, D is abs(SRC - DST).
% ===========================================
% ===========================================

% ===========================================
% DIRECTION
% ===========================================
% get_direction([SrcX, SrcY], [DestX, DestY], Direction)) : Coordinates in alpha mode
get_direction([R1, C1], [R2, C2], D) :-
        
        R1 == R2, C1 \= C2,
        convert_alpha_point([C1, C2], [Y1, Y2]),
        Y2 > Y1, D = southeast;

        R1 == R2, C1 \= C2,
        convert_alpha_point([C1, C2], [Y1, Y2]),
        Y2 < Y1, D = northwest;
        
        C1 == C2,  R1 \= R2,
        convert_alpha_point([R1, R2], [X1, X2]),
        X2 > X1, D = southwest;

        C1 == C2, R1 \= R2,
        convert_alpha_point([R1, R2], [X1, X2]),
        X2 < X1, D = northeast;
        
        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X2 - X1, IncY is Y2 - Y1, 
        IncX == IncY, X2 > X1, D = south;
        
        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X2 - X1, IncY is Y2 - Y1, 
        IncX == IncY, X2 < X1, D = north;
        
        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X1 - X2, IncY is Y2 - Y1, 
        IncX == IncY, X1 > X2, D = east;

        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X1 - X2, IncY is Y2 - Y1, 
        IncX == IncY, X1 < X2, D = west.
        
   

% ===========================================
% VERIFICATION OF TRAJECTS
% ===========================================
%       Coordinates in alpha
check_road(L, [P1,P2], [R1,C1], [T1, T2], Player, Count) :-
        
        % Finish it
        R1 == T1, C1 == T2, !,
                %write('--> can_capture()'), nl,
                can_capture(L, [P1,P2], [T1,T2], Player);

        get_direction([P1, P2], [T1, T2], D),
        is_valid_house(L, [P1,P2], [R1,C1], Player, Count),
        %write('Direction: '), write(D), nl,
        inc([R1,C1], [TR, TC], D),
        Count1 is Count + 1, !,
        check_road(L, [P1,P2], [TR, TC], [T1, T2], Player, Count1);
        
        !, fail.
      
% ===========================================
%   Checking stuff
% ===========================================
is_valid_house([_|_], [_,_], [_,_], _, 0) :- !.
        
is_valid_house(L, [P1,P2], [R,C], Player, Count) :-
        Count > 0,
                is_empty_piece(L, [R,C]);
        
        Count > 0,
                is_trench([P1,P2]),
                get_piece(L, [R, C], Piece),
                \+ check_piece_player(Piece, Player).

can_capture(L, [P1,P2], [T1,T2], Player) :-
        
        is_empty_piece(L, [T1,T2]);
        %write('it\'s soooo true'), nl;
        
        is_trench([P1,P2]), is_trench([T1,T2]), !, fail;
        
        is_trench([T1,T2]),
                get_piece(L, [T1, T2], Piece),
                \+ check_piece_player(Piece, Player),
                convert_alpha_point([P1, P2], [X, Y]),
                convert_to_grid_pos(X, Y, Row, _),
                %write('Row: '), write(Row), nl,
                in_enemy_turf(Player, Row);
        
        get_piece(L, [T1, T2], Piece),
        \+ check_piece_player(Piece, Player).

%
is_empty_piece(L, [R,C]) :-
        get_piece(L, [R,C], e).

% ===========================================
% Le Trench
% ===========================================
is_trench([R, C]) :-
        convert_alpha_point([R, C], [X, Y]),
        convert_to_grid_pos(X, Y, Row, _),
        Row == 8.

% ===========================================
%  lel
% ===========================================
inc([R,C], [NextR, NextC], D) :-
        
        % southeast
        D == southeast,
                get_next_letter(C, NextC, s),
                NextR = R;
                %write(NextR), nl;
        
        % northwest
        D == northwest, 
                get_next_letter(C, NextC, n),
                NextR = R;
                %write([NextR, NextC]), nl;
        
        % southwest
        D == southwest,
                get_next_letter(R, NextR, s),
                NextC = C;
                %write([NextR, NextC]), nl;
        
        % northeast
        D == northeast, 
                get_next_letter(R, NextR, n),
                NextC = C;
                %write([NextR, NextC]), nl;
        
        % south
        D == south, 
                get_next_letter(R, NextR, s),
                get_next_letter(C, NextC, s);
        
        % north
        D == north,
                get_next_letter(R, NextR, n),
                get_next_letter(C, NextC, n);
        
        % east
        D == east,
                get_next_letter(R, NextR, n),
                get_next_letter(C, NextC, s);
        
        % west
        D == west, 
                get_next_letter(R, NextR, s),
                get_next_letter(C, NextC, n).
                

% ===========================================
% ===========================================