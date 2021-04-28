nao_membro(_,[]).
nao_membro(E,[A|B]) :- E \== A, nao_membro(E,B).

insere_ordenado(L,[],[L]).
insere_ordenado(A,[B|Res],[A,B|Res]):- A < B.

insere_ordenado(A,[B|Res],[B|C]):- A>= B, insere_ordenado(A,Res,C).

junta_novo_aleatorio(L,I,S,R):- random_between(I,S,A),
                                nao_membro(A,L),
                                insere_ordenado(A,L,R).


n_aleatorios(0,_,_,[]).

n_aleatorios(N,I,S,L):- N > 0,
                        Novo is N -1,
                        n_aleatorios(Novo,I,S,Aux),
                        junta_novo_aleatorio(Aux,I,S,L).

chave_euromilhoes(Numeros,Estrelas):- n_aleatorios(5,0,50,Numeros),n_aleatorios(2,0,12,Estrelas).

comp_maior_lista(L,C):- comp_maior_lista(L,C,0).

comp_maior_lista([],C,C).

comp_maior_lista([A|B],C,Aux):- length(A,N), N > Aux, comp_maior_lista(B,C,N).

comp_maior_lista([A|B],C,Aux):- length(A,N), N < Aux, comp_maior_lista(B,C,Aux).  

duplica_elementos([],[]).

duplica_elementos([A|B],[A,A|Res]):-  duplica_elementos(B,Res).

insere_ordenado2(E,L,Res):-
    findall(X,(member(X,L),X < E),Menores),
    findall(X,(member(X,L),X > E),Maiores),
    append(Menores,[E|Maiores],Res).

junta_novo_aleatorio2(L,I,S,Res):-
                                random_between(I,S,A),
                                \+ member(A,L),
                                insere_ordenado2(A,L,Res).

repete_el(_,0,[]).
repete_el(E,N,[E|L]):- N > 0,Novo is N -1, repete_el(E,Novo,L).

repete_el2(E,N,L):- length(L,N), maplist(=(E),L).

duplica_elementos2(L,Res):- findall([E,E],member(E,L),Aux),
                            append(Aux,Res).

num_occ(L,E,C):- findall([E,E],member(E,L),B), length(B,C).

