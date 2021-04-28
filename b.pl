nao_membro(_,[]).

nao_membro(E,[A|B]):- E \== A, nao_membro(E,B).

pertence(E,[A,_]):- E == A.

pertence(E,[_,B]):- pertence(E,B).

nao_pertence(_,[]).

nao_pertence(E,[A|B]):- E \==A, nao_pertence(E,B).

diferenca([],A,A).

diferenca([A|Res],L,[A|B]):- pertence(A,L), diferenca(Res,L, B).
diferenca([A|Res],L,B):- nao_ pertence(A,L), diferenca(Res,L, B).

subconjunto([],_).

subconjunto([P|R],L),member(P,L),subconjunto(R,L).

mult([],_,[]).

mult([A|B],N,[Novo|L2]):- Novo is A*N, mult(B,N,L).
