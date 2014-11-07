% ========================
% Get Positions
% ========================

convert_to_grid_pos(R, C, Line, Col) :-
        S is R + C,
        S < 9, !,
        Line is S - 1,
        Col is C.

convert_to_grid_pos(R, C, Line, Col) :-
        S is R + C,
        Line is S - 1,
        Col is 9 - R.

% Rows
convert_alpha_num(a, 1).
convert_alpha_num(b, 2).
convert_alpha_num(c, 3).
convert_alpha_num(d, 4).
convert_alpha_num(e, 5).
convert_alpha_num(f, 6).
convert_alpha_num(g, 7).
convert_alpha_num(h, 8).

% Columns
convert_alpha_num(i, 1).
convert_alpha_num(j, 2).
convert_alpha_num(k, 3).
convert_alpha_num(l, 4).
convert_alpha_num(m, 5).
convert_alpha_num(n, 6).
convert_alpha_num(o, 7).
convert_alpha_num(p, 8).

% ========================
% ========================