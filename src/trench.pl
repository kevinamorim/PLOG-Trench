% Trench Game - PLOG exercise 
gameList([[g], [co, co], [ca, ca, ca], [sa, sa, sa, sa], 
          [e, so, so, so, e], [e, e, so, so, e, e], [e, e, e, so, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so, e, e, e], [e, e, so, so, e, e], [e, so, so, so, e], [sa, sa, sa, sa],
         [ca, ca, ca], [co, co], [g]]).

% Print game state

% Parameters: List to print, Starting line
% TODO: Verify if I is bigger than the matrix rows.
printGameState([], _).

printGameState([H|T], 7) :-
                      I2 is 7 * 2,
                      S is abs(14 - I2),
                      printLineSpaces(S);
                      printTrenchLine(H),
                      nl,
                      I1 is 7 + 1,
                      printGameState(T, I1).

printGameState([H|T], I) :-
                      I2 is I * 2,
                      S is abs(14 - I2),
                      printLineSpaces(S);
                      printGameLine(H),
                      nl,
                      I1 is I + 1,
                      printGameState(T, I1).
                                   

% Prints empty lines
% Parameters: S: number of line to print.
% Restrictions: S should be bigger than 0 (zero).
printLineSpaces(S) :-
                S > 0,
                write(' '),
                S1 is S - 1,
                printLineSpaces(S1).

% Prints the actual game line
printGameLine([]).

% Parameters: Game line to print (list)
printGameLine([H]) :-
                     getSymbol(H, S),
                     %use_module(library(clpfd)),
                     %put_code(S),
                     format('~c', S).

printGameLine([H|T]) :-
                     getSymbol(H, S),
                     %use_module(library(clpfd)),
                     %put_code(S),
                     format('~c', S),
                     write('   '),
                     printGameLine(T).

printTrenchLine([]).

printTrenchLine([H]) :-
                     getSymbol(H, S),
                     %use_module(library(clpfd)),
                     %put_code(S),
                     format('~c', S).

printTrenchLine([H|T]) :-
                     getSymbol(H, S),
                     %put_code(S),
                     format('~c', S),
                     write(' '),
                     format('~c', [215]),
                     write(' '),
                     printTrenchLine(T).
             

% Game symbols, can be changed to whatever we want 
% Returns: Char code, so we can use all the ascii chars (even extended)
getSymbol(g, 52).
getSymbol(co, 51).
getSymbol(ca, 50).
getSymbol(sa, 49).
getSymbol(so, 48).
getSymbol(e, 45).