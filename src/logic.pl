:- consult(draw).
:- consult(list_stuff).

% User input, just testing stuff...

play(X, Y) :- printTest(a), nl, nl, read_piece(X), read_destination(Y).


read_piece(X) :- write('Choose a piça: '), read(X), write('Well done, you choosed this PIÇA: '), write(X), nl.

read_destination(Y) :- write('Choose where to place your penis: '), read(Y), write('Ok, your penis is here: '), write(Y), nl.
                

% getPiece(Row, Column, GameList, Piece).
%       Row must be in:         [a, b, c, d, e, f, g, h]
%       Column must be in:      [i, j, k, l, m, n, o, p]
getPiece(R, C, L, P) :-
        convertAlphaToNum(R, R1),
        convertAlphaToNum(C, C1),
        write(R1), write(' '), write(C1), nl,
        selectElementAt(R1, C1, L, P).

getPiece(R, C) :-
        gameList(L),
        getPiece(R, C, L, P),
        write(P), nl.

% movePiece(Fr, Fc, Tr, Tc, GameList, Piece).
movePiece([X1, Y1], [X2, Y2], L, P) :-
        gameList(L),
        printGameState(L),
        convertAlphaToNum(X1, R1),
        convertAlphaToNum(Y1, C1),
        convertAlphaToNum(X2, R2),
        convertAlphaToNum(Y2, C2),
        write([R1, C1, R2, C2]), nl,
        selectElementAt(R1, C1, L, P),
        getSymbol(P, S1),
        write('Piece 1: '), write(S1), nl,
        selectElementAt(R2, C2, L, P2),   
        getSymbol(P2, S2),
        write('Piece 2: '), write(S2), nl.

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
