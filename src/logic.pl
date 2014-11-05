:- consult(draw).
:- consult(list_stuff).

% User input, just testing stuff...

play(X, Y) :- printTest(a), nl, nl, read_piece(X), read_destination(Y).


read_piece(X) :- write('Choose a piça: '), read(X), write('Well done, you choosed this PIÇA: '), write(X), nl.

read_destination(Y) :- write('Choose where to place your penis: '), read(Y), write('Ok, your penis is here: '), write(Y), nl.
                

% getPiece(Row, Column, GameList, Piece).
%       Row and Columns must be in:         [1, 2, 3, 4, 5, 6, 7, 8]
getPiece(L, [R, C], P) :-
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

% movePiece(GameList, [From], [To], NewGameList).

movePiece(L, [X1, Y1], [X2, Y2], NL) :-
        convertAlphaToNum(X1, R1),
        convertAlphaToNum(Y1, C1),
        convertAlphaToNum(X2, R2),
        convertAlphaToNum(Y2, C2),
        getPiece(L, [R1, C1], P),
        setPiece(L, e, [R1, C1], L1),
        setPiece(L1, P, [R2, C2], NL).

%
convertToGridPos(R, C, Line, Col) :-
        S is R + C,
        S < 9, !,
        Line is S - 1,
        Col is C.

convertToGridPos(R, C, Line, Col) :-
        S is R + C,
        Line is S - 1,
        Col is 9 - R.

% ========================
% Helpers
% Rows
convertAlphaToNum(a, 1).
convertAlphaToNum(b, 2).
convertAlphaToNum(c, 3).
convertAlphaToNum(d, 4).
convertAlphaToNum(e, 5).
convertAlphaToNum(f, 6).
convertAlphaToNum(g, 7).
convertAlphaToNum(h, 8).
% Columns
convertAlphaToNum(i, 1).
convertAlphaToNum(j, 2).
convertAlphaToNum(k, 3).
convertAlphaToNum(l, 4).
convertAlphaToNum(m, 5).
convertAlphaToNum(n, 6).
convertAlphaToNum(o, 7).
convertAlphaToNum(p, 8).
