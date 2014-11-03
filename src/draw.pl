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
                      printBoardLeftRef(7),
                      write('  '),
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


printHeader(_) :- printLineSpaces(16), write('A'), printLineSpaces(3), write('P'), nl.
printFooter(_) :- printLineSpaces(16), write('P'), printLineSpaces(3), write('A').


% ==========================================================================
%  Print Board Structure
% ==========================================================================

printBoardLeftRef(I) :- 
        I < 16,
             getSymbol(I, S),
             write(S).

printBoardRightRef(I) :-
        I < 16,
                X is (14 - I),
                getSymbol(X, S),
                write(S).

          
getSymbol(0, 'B').
getSymbol(1, 'C').
getSymbol(2, 'D').
getSymbol(3, 'E').
getSymbol(4, 'F').
getSymbol(5, 'G').
getSymbol(6, 'H').
getSymbol(7, ' ').

getSymbol(8, 'I').
getSymbol(9, 'J').
getSymbol(10, 'K').
getSymbol(11, 'L').
getSymbol(12, 'M').
getSymbol(13, 'N').
getSymbol(14, 'O').
getSymbol(15, ' ').

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

