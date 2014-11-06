% ==================================================================================
% ==================================================================================
% Description of rules for each piece of the game. 
% ==================================================================================
% ==================================================================================
% The pieces are described as follow: 
% g - general
% co - coronel
% ca - capitan
% sa - sargeant 
% so - soldier
% The number in front of the 'symbol' is the num of the player that own that piece. 

% ===============================
% ALLOWED DIRECTION
% ===============================
% Returns allowed directions for each piece.

% Player 1
getAllowedDirFor(g1, D) :- D == d.
getAllowedDirFor(g1, D) :- D == f.
getAllowedDirFor(g1, D) :- D == b.
getAllowedDirFor(g1, D) :- D == l.
getAllowedDirFor(g1, D) :- D == r.

getAllowedDirFor(co1, D) :- D == d.
getAllowedDirFor(co1, D) :- D == f.
getAllowedDirFor(co1, D) :- D == l.
getAllowedDirFor(co1, D) :- D == r.

getAllowedDirFor(ca1, D) :- D == d.
getAllowedDirFor(ca1, D) :- D == f.
getAllowedDirFor(ca1, D) :- D == b.

getAllowedDirFor(sa1, D) :- D == d.
getAllowedDirFor(sa1, D) :- D == f.

getAllowedDirFor(so1, D) :- D == d.

% Player 2
getAllowedDirFor(g2, D) :- D == d.
getAllowedDirFor(g2, D) :- D == f.
getAllowedDirFor(g2, D) :- D == b.
getAllowedDirFor(g2, D) :- D == l.
getAllowedDirFor(g2, D) :- D == r.

getAllowedDirFor(co2, D) :- D == d.
getAllowedDirFor(co2, D) :- D == f.
getAllowedDirFor(co2, D) :- D == l.
getAllowedDirFor(co2, D) :- D == r.

getAllowedDirFor(ca2, D) :- D == d.
getAllowedDirFor(ca2, D) :- D == f.
getAllowedDirFor(ca2, D) :- D == b.

getAllowedDirFor(sa2, D) :- D == d.
getAllowedDirFor(sa2, D) :- D == f.

getAllowedDirFor(so2, D) :- D == d.
% ===============================
% ===============================

% ===============================
% MAX DISTANCE
% ===============================
% Predicates that returns the maximum distance that each piece can move.
% Base case (in case an invalid piece is queried)
maxDistanceFor(_, M) :- M is 0.

% Player 1
maxDistanceFor(g1, M) :- M is 5.
maxDistanceFor(co1, M) :- M is 4.
maxDistanceFor(ca1, M) :- M is 3.
maxDistanceFor(sa1, M) :- M is 2.
maxDistanceFor(so1, M) :- M is 1.

% Player2
maxDistanceFor(g2, M) :- M is 5.
maxDistanceFor(co2, M) :- M is 4.
maxDistanceFor(ca2, M) :- M is 3.
maxDistanceFor(sa2, M) :- M is 2.
maxDistanceFor(so2, M) :- M is 1.
% ===============================
% ===============================