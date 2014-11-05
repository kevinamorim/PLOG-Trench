% Lists library module
:- use_module(library(lists)).


% Testing replacement, will be used for moving pieces.
% select(element to replace, list, element that will replace, resulting list)

% ====================================
%   select(c, [a, b, c, d, e], k, X).
% ====================================

% This works like a charm, lol (but, seriously, it works...)

% Select an element in a matrix
% selectElem(Row, Column, List, Element)
selectElem(R, C, L, E) :-
        convertToGridPos(R, C, Line, Col),
        nth1(Line, L, X),
        nth1(Col, X, E).

selectElem(R, C, L, E) :-
        convertToGridPos(R, C, Line, Col),
        nth1(Line, L, X),
        nth1(Col, X, E).

replace([_|T], 1, X, [X|T]).

replace([H|T], I, X, [H|R]) :-
        I > 1,
        NI is I - 1,
        replace(T, NI, X, R).

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

