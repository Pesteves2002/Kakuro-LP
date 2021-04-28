pertence(P, [Q | _]) :- P == Q.
pertence(P, [_ | R]) :- pertence(P, R).
