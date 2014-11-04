% Lists library module
:- use_module(library(lists)).


% Testing replacement, will be used for moving pieces.
% select(element to replace, list, element that will replace, resulting list)

% ====================================
%   select(c, [a, b, c, d, e], k, X).
% ====================================

% This works like a charm, lol (but, seriously, it works...)

% Select an element in a matrix
% selectElementAt(Row, Column, List, Element)
selectElementAt(_, _, [], _).

selectElementAt(R, C, L, E) :-
        %convertAlphaToNum(R, R1),
        %convertAlphaToNum(C, C1),
        nth0(R, L, X),
        nth0(C, X, E).

% I'm thinking of using this file to create lists management "methods" 

% ========================
% Helpers
% Rows
convertAlphaToNum(a, 0).
convertAlphaToNum(b, 1).
convertAlphaToNum(c, 2).
convertAlphaToNum(d, 3).
convertAlphaToNum(e, 4).
convertAlphaToNum(f, 5).
convertAlphaToNum(g, 6).
convertAlphaToNum(h, 7).
% Columns
convertAlphaToNum(i, 0).
convertAlphaToNum(j, 1).
convertAlphaToNum(k, 2).
convertAlphaToNum(l, 3).
convertAlphaToNum(m, 4).
convertAlphaToNum(n, 5).
convertAlphaToNum(o, 6).
convertAlphaToNum(p, 7).