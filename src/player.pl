% ==================================================================================
% ==================================================================================
% Predicates that handles the players
% ==================================================================================
% ==================================================================================

% Players pieces
% Returns the list of pieces for each player.
playerPieces(p1, [g1, co1, ca1, sa1, so1]).
playerPieces(p2, [g2, co2, ca2, sa2, so2]).

% checkPiecePlayer(piece, player)
% Checks the player that owns a given piece.
checkPiecePlayer(PI, P) :- playerPieces(P, L), member(PI, L), !.

players([p1,p2]).

first_player(p1).

next_player(p1, p2).
next_player(p2, p1).