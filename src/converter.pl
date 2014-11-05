% ========================
% Get Positions
% ========================

convertToGridPos(R, C, Line, Col) :-
        S is R + C,
        S < 9, !,
        Line is S - 1,
        Col is C.

convertToGridPos(R, C, Line, Col) :-
        S is R + C,
        Line is S - 1,
        Col is 9 - R.

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

% ========================
% ========================