perm([], []).
perm(L, [P | R]) :- escolhe(L, P, L1),
perm(L1, R).
