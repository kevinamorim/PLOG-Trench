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
selectElementAt(R, C, L, E) :-
        S is R + C,
        S < 9, !,
        Line is S - 1,
        Col is C,
        nth1(Line, L, X),
        nth1(Col, X, E).

selectElementAt(R, C, L, E) :-
        S is R + C,
        Line is S - 1,
        Col is 9 - R,
        nth1(Line, L, X),
        nth1(Col, X, E).

% I'm thinking of using this file to create lists management "methods" 