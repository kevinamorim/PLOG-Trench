/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

% Input / Output file

readPosition([A,B]) :-
        get_char(A),
        get_char(B),
        get_char(_).