:- consult(draw).
:- consult(list_stuff).
:- consult(converter).

% Print Game List: ? gameList(X), printGameState(X, 0). (command) 
printTest(_) :- gameList(X), printGameState(X). % Temporary ;)

% User input, just testing stuff...

play(X, Y) :- printTest(a), nl, nl, read_piece(X), read_destination(Y).


read_piece(X) :- write('Choose a piça: '), read(X), write('Well done, you choosed this PIÇA: '), write(X), nl.

read_destination(Y) :- write('Choose where to place your penis: '), read(Y), write('Ok, your penis is here: '), write(Y), nl.
                

% getPiece(GameList, [Row, Column], Piece).
%       Row must be in:         [a, b, c, d, e, f, g, h]
%       Column must be in:      [i, j, k, l, m, n, o, p]
getPiece(L, [R, C], P) :-
        convertAlphaToNum(R, R1),
        convertAlphaToNum(C, C1),
        selectElem(R1, C1, L, P).

% ===========================================
%       Pieces placement
% ===========================================
% setPiece(GameList, Piece, [Pos], NewGameList)

setPiece(L, P, [R, C], NL) :-
        nth1(R, L, X),
        convertToGridPos(R, C, Line, Col),
        replace(X, Col, P, Res),
        replace(L, Line, Res, NL).

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

% convertToDistance([SrcRow, SrcCol], [DestRow, DestCol])
% convertToDistance([R1, C1],  [R2, C2]) :- 

% movePiece(GameList, [From], [To], NewGameList).
movePiece(L, [X1, Y1], [X2, Y2], NL) :-
        convertAlphaToNum(X1, R1),
        convertAlphaToNum(Y1, C1),
        convertAlphaToNum(X2, R2),
        convertAlphaToNum(Y2, C2),
        getPiece(L, [X1, Y1], P),
        %canMove(L, P, [R1,C1], [R2,C2]),
        setPiece(L, e, [R1, C1], L1),
        setPiece(L1, P, [R2, C2], NL).




% ==============================
%       Game Initialization
% ==============================
% Returns the initial game list, with all the pieces in their initial position

% g - general
% co - coronel
% ca - capitan
% sa - sargeant 
% so - soldier

gameList([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
          [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).

initialize(X) :- gameList(X).

% ==============================
%       Game Over
% ==============================
gameOver(X) :- checkPlayer(X, 1), !, checkPlayer(X, 2).

% Check player: verifies if any piece of the passed player exists... 


                 

