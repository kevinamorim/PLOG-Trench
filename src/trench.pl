% Trench Game - PLOG exercise
gameList([[g], [co, co], [ca, ca, ca], [sa, sa, sa, sa], 
          [e, so, so, so, e], [e, e, so, so, e, e], [e, e, e, so, e, e, e],
         [e, e, e, so, e, e, e], [e, e, so, so, e, e], [e, so, so, so, e], [sa, sa, sa, sa],
         [ca, ca, ca], [co, co], [g]]).

% Print game state

printGameState([], _).


% Parameters: List to print, Starting line
% TODO: Verify if I is bigger than the matrix rows. 
printGameState([H|T], I) :-
                      S is abs(7 - I),
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
printGameLine([H|T]) :-
                     getSymbol(H, S),
                     %use_module(library(clpfd)),
                     %put_code(S),
                     write(S),
                     write(' '),
                     printGameLine(T).
             

% Game symbols, can be changed to whatever we want 
% Returns: Char code, so we can use all the ascii chars (even extended)
getSymbol(g, 'G').
getSymbol(co, 'C').
getSymbol(ca, 'C').
getSymbol(sa, 'S').
getSymbol(so, 'S').
getSymbol(e, '_').