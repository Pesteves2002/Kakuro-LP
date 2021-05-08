:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els,Soma,Combs):- bagof(L, (combinacao(N,Els,L), sumlist(L,Soma)), Combs),!.
  
combinacoes_soma(_, _,_, []).

permutacoes_soma(N, Els, Soma, Perms):- combinacoes_soma(N,Els,Soma,A),
                                        permutacoes(A,C),sort(C,Perms),!.

permutacoes([],[]).
permutacoes(Lista, Res) :- findall(P,(member(Perm,Lista),permutation(P,Perm)), Res).


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

permutacao_possivel_espaco(Perm, Esp, Espacos, _):- permutacoes_soma_espacos([Esp],B),
                                 B = [[_|D]|_],
                                 D = [E|_],
                                espacos_com_posicoes_comuns(Espacos, Esp, Aux),
                                    permutacoes_soma_espacos(Aux,A),
                                    apanhar_lista(A,J),
                                 permutacao_possivel_espaco(Perm, Esp, Espacos, _,E,J).

permutacao_possivel_espaco(_,_,_,_,[]).

permutacao_possivel_espaco(A, _, _, _,[A|_],J):- ola(A,J).
                                        


permutacao_possivel_espaco(A, Esp, Espacos, _,[Perm|Res],J):-  ola(Perm,J),
                                        permutacao_possivel_espaco(A, Esp, Espacos, _,Res,J).

permutacao_possivel_espaco(A, Esp, Espacos, _,[Perm|Res],J):- \+ ola(Perm,J),
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

permutacoes_possiveis_espaco(Espacos, _, Esp,Perms_poss):- bagof(Aux,permutacao_possivel_espaco(Aux, Esp, Espacos, _),B),
                                                            lista_espaco(Esp,A),append([A],[B],Perms_poss).

%permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):- permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Espacos).
permutacoes_possiveis_espacos([], [],_). 
permutacoes_possiveis_espacos([Esp|Espacos], [Perms|Perms_poss_esps],Todos):- permutacoes_possiveis_espaco(Todos, _, Esp,Perms),
                                                                    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Todos).

% numeros_comuns(Lst_Perms, Numeros_comuns)

numeros_comuns(L,Res) :- obter_vetor_comum(L,A),numeros_comuns(A,1,Res).

numeros_comuns([], _,[]).
numeros_comuns([P | R], N,[Espaco|Res]) :- N1 is N + 1,
                                P \== 0,
                                Espaco = (N,P),
                                numeros_comuns(R,N1,Res),!.

numeros_comuns([P | R], N,Res) :-
                                P == 0,
                                N1 is N + 1,
                                numeros_comuns(R,N1,Res).

obter_vetor_comum([A],A).
obter_vetor_comum([A|B],L):-  mesmo_sitio(Res,A,L), obter_vetor_comum(B,Res),!.

mesmo_sitio([],[],[]).

mesmo_sitio([B|L1],[B|L2],[B|Res]):-   mesmo_sitio(L1,L2,Res).

mesmo_sitio([B|L1],[D|L2],[0|Res]):- B \== D, mesmo_sitio(L1,L2,Res).

atribui_comuns([]).

atribui_comuns([A|Perms_Possiveis]):- nth1(1,A,D),nth1(2,A,L),
                                        numeros_comuns(L,Lista),
                                        Lista \== [],
                                        juntar(D,Lista),
                                        atribui_comuns(Perms_Possiveis).

atribui_comuns([A|Perms_Possiveis]):- A = [_|_],atribui_comuns(Perms_Possiveis),!.
                                    
% funcao auxiliar que substitui valores
juntar(_,[]).

juntar(D,[A|Lista]):- nth1(Pos,D,Valor), A = (Pos,Valor), juntar(D,Lista).



% obtem a lista de perms_ possiveis para espacos
obter_lista([],[]).

obter_lista([A|Res],[L|Resto]):- A = [B|C], C = [D|_],L = [B|D],  obter_lista(Res,Resto),!.


% verificar se o elmento duma lista e var ou nao

verificar([],[]).
verificar([A|Resto],[0|Res]):- var(A), verificar(Resto,Res),!.

verificar([A|Resto],[A|Res]):- nonvar(A), verificar(Resto,Res),!.

filtra([],_,[]).

filtra([A|Lst_Perms],Filtro,[A|Res]):- possivel(A,Filtro),  filtra(Lst_Perms,Filtro,Res),!.
filtra([A|Lst_Perms],Filtro,Res):- \+ possivel(A,Filtro),  filtra(Lst_Perms,Filtro,Res),!.


%verifica se a permutacao e possivel
possivel([],[]).

possivel([_|Res],[B|C]):- B == 0, possivel(Res,C),!.
possivel([A|Res],[B|C]):- B == A, possivel(Res,C),!.

% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)

retira_impossiveis(Perms_Possiveis,Res):- obter_lista(Perms_Possiveis,L),tudo(L,Res),!.

tudo([],[]).

tudo([F|List],[M|Res]):- F = [A|B],
                verificar(A,Filtro),
                filtra(B,Filtro,J), M = [A|[J]],tudo(List,Res),!.

% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Caso de paragem

simplifica(Perms_Possiveis, Perms_Possiveis):- atribui_comuns(Perms_Possiveis),
                                             retira_impossiveis(Perms_Possiveis,Res),
                                              Res == Perms_Possiveis,!.

simplifica(Perms, Novas) :-
                        atribui_comuns(Perms),
                        retira_impossiveis(Perms, Res),
                        simplifica(Res, Novas).

% inicializa(Puzzle, Perms_Possiveis)

inicializa(Puzzle, Res):- espacos_puzzle(Puzzle, Espacos),
                        permutacoes_possiveis_espacos(Espacos, Perms_Possiveis),
                        simplifica(Perms_Possiveis,Res).

% escolhe_menos_alternativas(Perms_Possiveis, Escolha)

% escolher([[[X,Y],[[1,2],[3,4]]],[[X,Y],[[1,2]]],[[X,Y],[[5,6],[7,9]]]],Res,X).

escolhe_menos_alternativas(Perms_Possiveis, Escolha):- escolher(Perms_Possiveis,[Escolha|_],_).

ler_tamanho(L,Res):- L = [_,B],length(B,Res).

escolher([],[],999999).
escolher([A|L],Res,Antigo):- ler_tamanho(A,Tam),Tam == 1,!, escolher(L,Res,Antigo).
escolher([A|L],[A|Res],Tam):- ler_tamanho(A,Tam),  escolher(L,Res,Antigo),Antigo >= Tam,!.
escolher([A|L],Res,Antigo):- ler_tamanho(A,Tam),  escolher(L,Res,Antigo),Antigo < Tam,!.

% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis):- Escolha = [Esp, Lst_Perms],
                                                            member(Perm,Lst_Perms),
                                                            Esp = Perm,
                                                            maplist(igual_escolha(Escolha),Perms_Possiveis,Novas_Perms_Possiveis),!.

igual_escolha(Escolha,E,A):- Escolha == E, Escolha = [Esp|_] ,A = [Esp,[Esp]],!.

igual_escolha(Escolha,E,E):- Escolha \== E.

% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)

% escolhe_menos_alternativas(Perms_Possiveis, Escolha)

% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)

% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)


resolve_aux(Perms_poss, Novas_Perms_Possiveis):- escolhe_menos_alternativas(Perms_poss, Escolha),
                                        experimenta_perm(Escolha, Perms_poss, A),
                                        simplifica(A, B),
                                        resolve_aux(B,Novas_Perms_Possiveis),!.

resolve_aux(Perms_Possiveis, A):- simplifica(Perms_Possiveis,A).



