:- consult(draw).
:- consult(list_stuff).
:- consult(converter).

% ===========================================
%       Pieces getter
% ===========================================
% getPiece(GameList, [Row, Column], Piece).
%       Row and Columns must be in:         [1, 2, 3, 4, 5, 6, 7, 8]
getPiece(L, [R,C], P) :-
        convertToGridPos(R, C, Row, Col),
        selectElem(Row, Col, L, P).

% ===========================================
%       Pieces placement
% ===========================================
% setPiece(GameList, Piece, [Pos], NewGameList)
setPiece(L, P, [R, C], NL) :-
        convertToGridPos(R, C, Row, Col),
        nth1(Row, L, X),
        replace(X, Col, P, Res),
        replace(L, Row, Res, NL).

% ===========================================
%       Pieces movement
% ===========================================
% canMove(GameList, Piece, [From], [To])

canMove(_, _, _, _). % !REMOVE!

% canMove(L, g1, [X1, Y1], [X2, Y2]).
% canMove(L, g2, [X1, Y1], [X2, Y2]).

% canMove(L, co1, [X1, Y1], [X2, Y2]).
% canMove(L, co2, [X1, Y1], [X2, Y2]).

% canMove(L, ca1, [X1, Y1], [X2, Y2]).
% canMove(L, ca2, [X1, Y1], [X2, Y2]).

% canMove(L, sa1, [X1, Y1], [X2, Y2]).
% canMove(L, sa2, [X1, Y1], [X2, Y2]).

% canMove(L, so1, [X1, Y1], [X2, Y2]).
% canMove(L, so2, [X1, Y1], [X2, Y2]).


% ===============================
% DISTANCE
% ===============================
% getDistance([SrcRow, SrcCol], [DestRow, DestCol], distance)
% Checks first if the movement is diagonal or perpendicular. 
% If only one of the coordinates changes: diagonal
% If both coordinates changes: perpendicular
% We use a generic method for calculating the distance, passing to it the two coordinates that will 
%   make the distance.
getDistance([R1, C1], [R2, C2], D) :- 
        R1 == R2, C1 \== C2, convertAlphaToNum(C1, X), convertAlphaToNum(C2, Y), calculateDistance(X, Y, D).

getDistance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 == C2, convertAlphaToNum(R1, X), convertAlphaToNum(R2, Y), calculateDistance(X, Y, D).

getDistance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 \== C2, convertAlphaToNum(R1, X), convertAlphaToNum(R2, Y), calculateDistance(X, Y, D).

% Calculates the distance between two coordinates.
calculateDistance(SRC, DST, D) :-
        SRC \== DST, D is abs(SRC - DST).
% ===============================
% ===============================

% ===============================
% MAX DISTANCE
% ===============================
% Predicates that returns the maximum distance that each piece can move.

% Base case
maxDistanceFor(_, M) :- M is 0.

% Player 1
maxDistanceFor(g1, M) :- M is 5.
maxDistanceFor(co1, M) :- M is 4.
maxDistanceFor(ca1, M) :- M is 3.
maxDistanceFor(sa1, M) :- M is 2.
maxDistanceFor(so1, M) :- M is 1.

% Player2
maxDistanceFor(g2, M) :- M is 5.
maxDistanceFor(co2, M) :- M is 4.
maxDistanceFor(ca2, M) :- M is 3.
maxDistanceFor(sa2, M) :- M is 2.
maxDistanceFor(so2, M) :- M is 1.

% ===============================
% ===============================
        
% movePiece(GameList, [From], [To], NewGameList).

movePiece(L, [X1,Y1], [X2,Y2], NL) :-
        convertAlphaToNum(X1, R1),
        convertAlphaToNum(Y1, C1),
        convertAlphaToNum(X2, R2),
        convertAlphaToNum(Y2, C2),
        getPiece(L, [R1, C1], P),
        setPiece(L, e, [R1, C1], L1),
        setPiece(L1, P, [R2, C2], NL).
