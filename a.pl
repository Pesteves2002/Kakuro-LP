:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els,Soma,Combs):- bagof(L, combinacao(N,Els,L), A),
                                      setof(X, (member(X,A),sumlist(X,Soma)),Combs),!.
combinacoes_soma(_, _,_, []).

permutacoes_soma(N, Els, Soma, Perms):- combinacoes_soma(N,Els,Soma,A),
                                        permutacoes(A,C),sort(C,Perms),!.

permutacoes([],[]).
permutacoes([A|B], Sol) :- bagof(P, permutation(A,P), Res),
                           append(C,Res,Sol),
                           permutacoes(B,C), !.



% espaco_fila(Fila,Esp,H_V)

%  Fila = [[5, 13], _, _, [0, 0], [6, 9], _, _, _], espaco_fila(Fila, Esp, h) .


cria_espaco([A|Res],v,espaco(W,Res)):- A = [W|_].

cria_espaco([A|Res],h,espaco(W,Res)):- A = [_,W|_].

lista_espaco(espaco(_,Lista),Lista).
numero_espaco(espaco(Num,_),Num).


% caso de paragem
espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                           length(C,Tam),
                           Tam == 1,
                           Fila = [A|B],
                           A \== [0,0],
                           B \== [],
                           cria_espaco(Fila,H_V,Esp),!.

%criar espaco
espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                           C = [_,Z|_],
                           split(Fila,Z,F,_),
                           F = [A|B],
                           A \== [0,0],
                           B \== [],
                           cria_espaco(F,H_V,Esp).

% adicionar a lista se Z for diferente de [0,0]
espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                              C = [_,Z|_],
                              split(Fila,Z,_,B),
                              Z \== [0,0],
                              espaco_fila([Z|B],Esp,H_V).

% passar a lista se Z for igual a [0,0]

espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                           C = [_,Z|_],
                           split(Fila,Z,_,B),
                           Z == [0,0],
                           espaco_fila(B,Esp,H_V).

% funcao auxilliar para separar uma lista
split([X|T],E,[],T):- X == E.

split([X|T], E, [X|LL], LR) :-
    X \== E,
    split(T, E, LL, LR).


espacos_fila(H_V,Fila,Esp):- bagof(Aux,espaco_fila(Fila,Aux,H_V),Esp),!.
espacos_fila(_,_,[]).


% espacos_puzzle(Puzzle,Espacos)
le_puzzle([],[],_).

le_puzzle([A|Res],Junto,H_V):- espacos_fila(H_V,A,Resultado),Resultado \== [],append(Resultado,Espacos,Junto),le_puzzle(Res,Espacos,H_V),!.
le_puzzle([A|Res],Espacos,H_V):- espacos_fila(H_V,A,Resultado),Resultado == [],le_puzzle(Res,Espacos,H_V),!.

espacos_puzzle(Fila,Res):- le_puzzle(Fila,A,h), mat_transposta(Fila,Transposta), le_puzzle(Transposta,B,v),append(A,B,Res).


%  espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
espacos_com_posicoes_comuns([],_,[]).


espacos_com_posicoes_comuns([A|Res],Esp,Esps_com):- lista_espaco(A,Lst),
                                                     lista_espaco(Esp,Num),
                                                     \+ ver_comuns(Lst,Num),!,
                                                     espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

espacos_com_posicoes_comuns([A|Res],Esp,[A|Esps_com]):- lista_espaco(A,Lst),
                                                     lista_espaco(Esp,Num),
                                                      ver_comuns(Lst,Num),
                                                      A \== Esp,
                                                      espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

espacos_com_posicoes_comuns([A|Res],Esp,Esps_com):- A == Esp,
                                                    espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.


% para cada elemento da lista.

membro(B,[A|_]):- B == A,!.
membro(B,[_|Res]):- membro(B,Res).

ver_comuns([A|_],L):- membro(A,L).
ver_comuns([_|B],L):- ver_comuns(B,L).

% permutacoes_soma_espacos([espaco(3,[P24,P25])], Perms_soma).
% permutacoes_soma_espacos(Espacos, Perms_soma)
permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([A|Res],[Novo|Final]):- lista_espaco(A,Lst),
                                                length(Lst,N),
                                                numero_espaco(A,Soma),
                                                permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
                                                append([A],[Perms],Novo),
                                                permutacoes_soma_espacos(Res,Final).

% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)

permutacao_possivel_espaco(Perm, Esp, Espacos, _,A,B,F,E,J) :- espacos_com_posicoes_comuns(Espacos, Esp, Perm),
                                                        permutacoes_soma_espacos(Perm,A),
                                                            permutacoes_soma_espacos([Esp],B),A = [C|_],
                                                            C = [_|F],
                                                            B = [[_|D]|_],D = [E|_],
                                                            apanhar_lista(A,J),
                                                            ola([9,8],J),writeln(sucesso).
                                                            
                                                            

% ver se o elemento pertence a pelo menos um conjunto de listas
pertence_a_1_lista(El, Listas) :-member(Lista, Listas),
                    member(El, Lista),!.



% ola([1,9],[[1,2,3],[9,8,7]]).
% ver se uma lista pertence a uma quantidade de listas
ola([],[]).
ola([A|Perms],[B|Espacos]):- pertence_a_1_lista(A,B),ola(Perms,Espacos).

% faz a lista de lista com espacos possiveis
apanhar_lista([],[]).
apanhar_lista([A|Lista],Novo):- A= [_|Perms],append(Perms,Res,Novo),apanhar_lista(Lista,Res).