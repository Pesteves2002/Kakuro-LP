:- [codigo_comum,puzzles_publicos].

%-------------------------------------------------------------------------------
combinacoes_soma(N, Els,Soma,Combs):- bagof(L, (combinacao(N,Els,L), sumlist(L,Soma)), Combs),!.
  
combinacoes_soma(_, _,_, []).

permutacoes_soma(N, Els, Soma, Perms):- combinacoes_soma(N,Els,Soma,A),
                                        permutacoes(A,C),sort(C,Perms),!.

permutacoes([],[]).
permutacoes(Lista, Res) :- findall(P,(member(Perm,Lista),permutation(P,Perm)), Res).

%-------------------------------------------------------------------------------


% espaco_fila(Fila,Esp,H_V)

%  Fila = [[5, 13], _, _, [0, 0], [6, 9], _, _, _], espaco_fila(Fila, Esp, h) .


cria_espaco([A|Res],v,espaco(W,Res)):- A = [W|_].

cria_espaco([A|Res],h,espaco(W,Res)):- A = [_,W|_].

lista_espaco(espaco(_,Lista),Lista).

numero_espaco(espaco(Num,_),Num).

%-------------------------------------------------------------------------------

% espaco_fila(Fila,Esp,H_V)

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

%-------------------------------------------------------------------------------

espacos_fila(H_V,Fila,Esp):- bagof(Aux,espaco_fila(Fila,Aux,H_V),Esp),!.
espacos_fila(_,_,[]).

%-------------------------------------------------------------------------------

espacos_puzzle(Fila,Res):- le_puzzle(Fila,A,h),
                            mat_transposta(Fila,Transposta),
                            le_puzzle(Transposta,B,v),
                            append(A,B,Res).

% funcao auxiliar, ler um puzzle
le_puzzle([],[],_).
% caso de a lista ter espacos
le_puzzle([A|Res],Junto,H_V):- espacos_fila(H_V,A,Resultado),
                                Resultado \== [],
                                append(Resultado,Espacos,Junto),
                                le_puzzle(Res,Espacos,H_V),!.
% caso de a lista nao ter espacos
le_puzzle([A|Res],Espacos,H_V):- A = [_|_],le_puzzle(Res,Espacos,H_V),!.

%-------------------------------------------------------------------------------

%  espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
espacos_com_posicoes_comuns([],_,[]).

espacos_com_posicoes_comuns([A|Res],Esp,Esps_com):- A == Esp,
                                                    espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

espacos_com_posicoes_comuns([A|Res],Esp,Esps_com):- lista_espaco(A,Lst),
                                                     lista_espaco(Esp,Num),
                                                     \+ ver_comuns(Lst,Num),!,
                                                     espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

espacos_com_posicoes_comuns([A|Res],Esp,[A|Esps_com]):- espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

% para cada elemento da lista.

membro(B,[A|_]):- B == A,!.
membro(B,[_|Res]):- membro(B,Res).

ver_comuns([A|_],L):- membro(A,L).
ver_comuns([_|B],L):- ver_comuns(B,L).

%-------------------------------------------------------------------------------

% permutacoes_soma_espacos([espaco(3,[P24,P25])], Perms_soma).
% permutacoes_soma_espacos(Espacos, Perms_soma)
permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([A|Res],[Novo|Final]):- lista_espaco(A,Lst),
                                                length(Lst,N),
                                                numero_espaco(A,Soma),
                                                permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
                                                append([A],[Perms],Novo),
                                                permutacoes_soma_espacos(Res,Final).

%-------------------------------------------------------------------------------


permutacao_possivel_espaco(Perm, Esp, Espacos, _):- permutacoes_soma_espacos([Esp],Lista),
                                Lista = [[_,D]|_],
                                espacos_com_posicoes_comuns(Espacos, Esp, Aux),
                                permutacoes_soma_espacos(Aux,A),
                                apanhar_lista(A,J),
                                permutacao_possivel_espaco(Perm, Esp, Espacos, _,D,J).

% permutacao_possivel_espaco(Perm, Esp, Espacos, _,D,J)

permutacao_possivel_espaco(A, _, _, _,[A|_],J):- ola(A,J).
                                        
permutacao_possivel_espaco(A, Esp, Espacos, _,[Perm|Res],J):- Perm = [_|_],
                                        permutacao_possivel_espaco(A, Esp, Espacos, _,Res,J).


% ver se o elemento pertence a pelo menos um conjunto de listas
pertence_a_1_lista(El, Listas) :-member(Lista, Listas),
                    member(El, Lista),!.

% ver se uma lista pertence a uma quantidade de listas
ola([],[]).
ola([A|Perms],[B|Espacos]):- pertence_a_1_lista(A,B),ola(Perms,Espacos).

% faz a lista de lista com espacos possiveis
apanhar_lista([],[]).
apanhar_lista([A|Lista],Novo):- A = [_|Perms],append(Perms,Res,Novo),apanhar_lista(Lista,Res).

% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss)

permutacoes_possiveis_espaco(Espacos, _, Esp,[Lista|[Perms]]):- 
                        bagof(Aux,permutacao_possivel_espaco(Aux, Esp, Espacos, _),Perms),
                        lista_espaco(Esp,Lista).

%-------------------------------------------------------------------------------

%permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):- 
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Espacos).

permutacoes_possiveis_espacos([], [],_). 

permutacoes_possiveis_espacos([Esp|Espacos], [Perms|Perms_poss_esps],Todos):- 
        permutacoes_possiveis_espaco(Todos, _, Esp,Perms),
        permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Todos).

%-------------------------------------------------------------------------------

% numeros_comuns

%-------------------------------------------------------------------------------

atribui_comuns([]).

atribui_comuns([A|Perms_Possiveis]):- nth1(2,A,L),
                                        numeros_comuns(L,Lista),
                                        Lista \== [],
                                        append(L,M),
                                        nth1(1,A,D),
                                        D = M,
                                        atribui_comuns(Perms_Possiveis),!.

atribui_comuns([_|Perms_Possiveis]):- atribui_comuns(Perms_Possiveis).
                                    
% funcao auxiliar que substitui valores
juntar(_,[]).

juntar(D,[A|Lista]):-  A = (Pos,Valor),nth1(Pos,D,Valor), juntar(D,Lista).

%-------------------------------------------------------------------------------



numeros_comuns(L,Lst_comuns):- length(L,Tam),Tamanho is Tam +1,  nth1(1,L,Ref), findall(Aux,(between(1,Tamanho,Index),nth1(Index,Ref,Valor),compara(L,Index,Valor), Aux = (Index,Valor)),Lst_comuns).


compara([],_,_).

compara([A|B],Index,Valor):- nth1(Index,A,Valor),compara(B,Index,Valor).