% nao_membro(E, L) - E n�o � membro da lista L.
nao_membro(_, []).
nao_membro(X, [P|R]) :- X\=P, nao_membro(X, R).
