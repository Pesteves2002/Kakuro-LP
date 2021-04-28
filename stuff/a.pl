:_hi.

nao_membro(_,[]).
nao_membro(E,[A|B]) :- E \== A, nao_membro(E,B).