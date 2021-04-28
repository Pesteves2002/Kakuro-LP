nao_pertence(_,[]).
nao_pertence(P, [E | R]) :- E \== P ,nao_pertence(P, R).
