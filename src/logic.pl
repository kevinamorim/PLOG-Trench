:- consult(draw).

% User input, just testing stuff...

play(X, Y) :- printTest(a), nl, nl, read_piece(X), read_destination(Y).


read_piece(X) :- write('Choose a pi�a: '), read(X), write('Well done, you choosed this PI�A: '), write(X), nl.

read_destination(Y) :- write('Choose where to place your penis: '), read(Y), write('Ok, your penis is here: '), write(Y), nl.
                



