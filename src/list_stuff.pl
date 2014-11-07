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
selectElem(Row, Col, L, E) :-
        %convertToGridPos(R, C, Line, Col),
        nth1(Row, L, X),
        nth1(Col, X, E).

selectElem(Row, Col, L, E) :-
        %convertToGridPos(R, C, Line, Col),
        nth1(Row, L, X),
        nth1(Col, X, E).

% replaces the list element in the index I by X
replace([_|T], 1, X, [X|T]).

replace([H|T], I, X, [H|R]) :-
        I > 1,
        NI is I - 1,
        replace(T, NI, X, R).

% verifies if an element is member of a matrix (row by row)
member_matrix(_, []) :- fail.

member_matrix(X, [M|T]) :-
        member(X, M);
        member_matrix(X, T).
