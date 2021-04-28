pertence(P,[Q|_]) :- P == Q.
pertence(E,[_|R]):- pertence(E,R).

nao_pertence(_,[]).
nao_pertence(E,[P|R]) :- E \= P, nao_pertence(E,R).

diferenca([], _, []).
diferenca([P | R], L2, D) :-
pertence(P, L2),
diferenca(R, L2, D).
diferenca([P | R], L2, [P | D]) :-
nao_pertence(P, L2),
diferenca(R, L2, D).

escreve_lista(L):- escreve_lista(L,1).
escreve_lista([],_).
escreve_lista([A|R],N):- write('['), write(N), write(']: '), writeln(A), N1 is N +1,escreve_lista(R,N1).

mult([],_,[]).
mult([A|R],N,[A|L2]):- A is A*N , mult(R,N,L2).

produto_iter([P | R], Prod_act, Prod) :-
Novo_Prod_act is Prod_act * P,
produto_iter(R, Novo_Prod_act, Prod).