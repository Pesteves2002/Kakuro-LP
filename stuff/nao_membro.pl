% nao_membro(E, L) - E não é membro da lista L.
nao_membro(_, []).
nao_membro(X, [P|R]) :- X\=P, nao_membro(X, R).
