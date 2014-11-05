% ==================================================================================================
% ==================================================================================================
%                                          ->|DRAWING|<-
% ==================================================================================================
% ==================================================================================================

% Print Game List: ? gameList(X), printGameState(X, 0). (command) 
printTest(_) :- gameList(X), printGameState(X). % Temporary ;)


% Print game state

% Parameters: List to print, Starting line
% TODO: Verify if I is bigger than the matrix rows.
printGameState([], _).

printGameState([H|T], 7) :-
                      I2 is 7 * 2,
                      S is abs(14 - I2),
                      printLineSpaces(S),
                      write('   '),
                      printTrenchLine(H),
                      nl,
                      I1 is 7 + 1,
                      printGameState(T, I1).

printGameState([H|T], I) :-
                      I2 is I * 2,
                      S is abs(14 - I2),
                      printLineSpaces(S),
                      printBoardLeftRef(I),
                      write('  '),
                      printGameLine(H),
                      write('  '),
                      printBoardRightRef(I),
                      nl,
                      I1 is I + 1,
                      printGameState(T, I1).

printGameState([H|T]) :- printHeader(_), printGameState([H|T], 0), printFooter(_).
                                   

% Prints empty lines
% Parameters: S: number of line to print.
% Restrictions: S should be bigger than 0 (zero).
printLineSpaces(0).

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
                     write(S).

printGameLine([H|T]) :-
                     getSymbol(H, S),
                     write(S),
                     write(' '),
                     printGameLine(T).

printTrenchLine([]).

printTrenchLine([H]) :-
                     getSymbol(H, S),
                     write(S).

printTrenchLine([H|T]) :-
                     getSymbol(H, S),
                     write(S),
                     format('~c', [215]),
                     printTrenchLine(T).


printHeader(_) :- printLineSpaces(16), write('a'), printLineSpaces(3), write('i'), nl.
printFooter(_) :- printLineSpaces(16), write('p'), printLineSpaces(3), write('h'), nl.


% ==========================================================================
%  Print Board Structure
% ==========================================================================

printBoardLeftRef(I) :- 
        I < 7, !,
                X is I + 1,
                getSymbol(X, S),
                write(S).

printBoardLeftRef(I) :- 
        I < 16, !,
                getSymbol(I, S),
                write(S).

printBoardRightRef(I) :-
        I < 7, !,
                X is I + 9,
                getSymbol(X, S),
                write(S).

printBoardRightRef(I) :-
        I < 16, !,
                X is abs(8 - I),
                getSymbol(X, S),
                write(S).


getSymbol(0, a).
getSymbol(1, b).
getSymbol(2, c).
getSymbol(3, d).
getSymbol(4, e).
getSymbol(5, f).
getSymbol(6, g).
getSymbol(7, h).

getSymbol(8, i).
getSymbol(9, j).
getSymbol(10, k).
getSymbol(11, l).
getSymbol(12, m).
getSymbol(13, n).
getSymbol(14, o).
getSymbol(15, p).

% ==========================================================================
% ==========================================================================    
             
% ==========================================================================
% Game symbols, can be changed to whatever we want 
% Returns: String containing the symbol
% ==========================================================================

% Player 1
getSymbol(g1, '(5)').
getSymbol(co1, '(4)').
getSymbol(ca1, '(3)').
getSymbol(sa1, '(2)').
getSymbol(so1, '(1)').

% Player 2
getSymbol(g2, '[5]').
getSymbol(co2, '[4]').
getSymbol(ca2, '[3]').
getSymbol(sa2, '[2]').
getSymbol(so2, '[1]').

% Empty space
getSymbol(e, ' - ').

% ==========================================================================
% ==========================================================================

% g - general
% co - coronel
% ca - capitan
% sa - sargeant 
% so - soldier

% Trench Game List
gameList([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
          [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).

