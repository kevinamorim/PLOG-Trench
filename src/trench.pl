% Trench Game - PLOG exercise
gameList([a1] , [a2, a3]).

% Print game state
printGameState([H|T], I) :-
                      S is abs(7 - I),
                      printLineSpaces(S);
                      printGameLine(H),
                      nl,
                      I1 is I + 1,
                      printGameState(T, I1).
                                   
printGameState([], I).

printLineSpaces(S) :-
                S > 0,
                write(' '),
                S1 is S - 1,
                printLineSpaces(S1).

printGameLine([H|T]) :-
                     getSymbol(H, S),
                     format('~c', S),
                     write(' '),
                     printGameLine(T).
             
printGameLine([]).

getSymbol(a, 232).
getSymbol(b, 229).
getSymbol(c, 234).