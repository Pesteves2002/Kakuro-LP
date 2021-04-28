% membro(E, L) - E é membro da lista L.
membro(X,[X|R]).
membro(X,[P|R]) :- membro(X,R).