% Tomas Esteves - ist199341
:- [codigo_comum, puzzles_publicos].

%-------------------------------------------------------------------------------
%                   combinacoes_soma(N, Els, Soma, Combs)
% N é um inteiro, Els é uma lista de inteiros, e Soma é um inteiro,
% significa que Combs é a lista ordenada cujos elementos são as combinacoes N a N,
% dos elementos de Els cuja soma é Soma .
%-------------------------------------------------------------------------------


combinacoes_soma(N, Els,Soma,Combs):- bagof(L, combinacao(N,Els,L), A),findall(X, (member(X,A),sumlist(X,Soma)),Combs),!.
combinacoes_soma(_, _,_, []).

%-------------------------------------------------------------------------------
%                   permutacoes_soma(N, Els, Soma, Perms),
% em que N é um inteiro, Els é uma lista de inteiros, e Soma é um inteiro, 
% significa que Perms é a lista ordenada cujos elementos são as permutações das combinações N a N, 
% dos elementos de Els cuja soma éSoma .
%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- combinacoes_soma(N,Els,Soma,A),permutacoes(A,C),sort(C,Perms),!.

permutacoes([],[]).
permutacoes([A|B], Sol) :-
   bagof(P, permutation(A,P), Res),append(C,Res,Sol), permutacoes(B,C),!.

%-------------------------------------------------------------------------------
%                      espaco_fila(Fila, Esp, H_V),
% em que Fila é uma fila (linha ou coluna) de um puzzle e H_V é um dos átomos h ou v,
% conforme se trate de uma fila horizontal ou vertical, respectivamente,
% significa que Esp é um espaço de Fila, tal como descrito na Secção 2.1, no passo 1.
%-------------------------------------------------------------------------------

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


%-------------------------------------------------------------------------------
%                       espacos_fila(H_V, Fila, Espacos)
%  Fila é uma fila (linha ou coluna) de uma grelha e
%  H_V é um dos átomos h ou v, significa que Espacos é
% a lista de todos os espaços de Fila, da esquerda para a direita.
%-------------------------------------------------------------------------------

espacos_fila(H_V,Fila,Esp):- bagof(Aux,espaco_fila(Fila,Aux,H_V),Esp),!.
espacos_fila(_,_,[]).

%-------------------------------------------------------------------------------
%                       espacos_puzzle(Puzzle, Espacos)
% Puzzle é um puzzle, significa que Espacos é a lista de espaços de Puzzle,
% tal como descrito na Secção 2.1, no passo 1.
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
le_puzzle([A|Res],Espacos,H_V):- espacos_fila(H_V,A,Resultado),
                                Resultado == [],
                                le_puzzle(Res,Espacos,H_V),!.

%-------------------------------------------------------------------------------
%             espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Espacos é uma lista de espaços e Esp é um espaço,
% significa que Esps_com é a lista de espaços com variáveis em comum com Esp, exceptuando Esp.
% Os espaços em Esps_com devem aparecer pela mesma ordem que aparecem em Espacos.
%-------------------------------------------------------------------------------

espacos_com_posicoes_comuns([],_,[]).

% caso de nao ter casas em comum
espacos_com_posicoes_comuns([A|Res],Esp,Esps_com):- lista_espaco(A,Lst),
                                                     lista_espaco(Esp,Num),
                                                     \+ ver_comuns(Lst,Num),!,
                                                     espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

% caso de ter casas em comum
espacos_com_posicoes_comuns([A|Res],Esp,[A|Esps_com]):- lista_espaco(A,Lst),
                                                     lista_espaco(Esp,Num),
                                                      ver_comuns(Lst,Num),
                                                      A \== Esp,
                                                      espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.
% caso de ser igual ao proprio espaco
espacos_com_posicoes_comuns([A|Res],Esp,Esps_com):- A == Esp,
                                                    espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

%funcoes auxiliares
membro(B,[A|_]):- B == A,!.
membro(B,[_|Res]):- membro(B,Res).

ver_comuns([A|_],L):- membro(A,L).
ver_comuns([_|B],L):- ver_comuns(B,L).

%-------------------------------------------------------------------------------
%             permutacoes_soma_espacos(Espacos, Perms_soma)
% Espacos é uma lista de espaços, 
% significa que Perms_soma é a lista de listas de 2 elementos,
% em que o 1o elemento é um espaço de Espacos e 
% o 2o é a lista ordenada de permutações cuja soma é igual à soma do espaço
%-------------------------------------------------------------------------------

permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([Esp|Res_espacos],[Novo|Perms_soma]):- lista_espaco(Esp,Lst),
                                                length(Lst,N),
                                                numero_espaco(Esp,Soma),
                                                permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
                                                append([Esp],[Perms],Novo),
                                                permutacoes_soma_espacos(Res_espacos,Perms_soma).

%-------------------------------------------------------------------------------
%             permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perm é uma permutação, Esp é um espaço, Espacos é uma lista de espaços, e
% Perms_soma é uma lista de listas tal como obtida pelo predicado anterior,
% significa que Perm é uma permutação possível para o espaço Esp.
%-------------------------------------------------------------------------------

permutacao_possivel_espaco(Perm, Esp, Espacos, _):- permutacoes_soma_espacos([Esp],B),
                                 B = [[_|D]|_],
                                 D = [E|_],
                                espacos_com_posicoes_comuns(Espacos, Esp, Aux),
                                    permutacoes_soma_espacos(Aux,A),
                                    apanhar_lista(A,J),
                                 permutacao_possivel_espaco(Perm, Esp, Espacos, _,E,J).

permutacao_possivel_espaco(_,_,_,_,[]).

permutacao_possivel_espaco(A, _, _, _,[A|_],J):- verificar_se_pertence(A,J).
                                        


permutacao_possivel_espaco(A, Esp, Espacos, _,[Perm|Res],J):-  verificar_se_pertence(Perm,J),
                                        permutacao_possivel_espaco(A, Esp, Espacos, _,Res,J).

permutacao_possivel_espaco(A, Esp, Espacos, _,[Perm|Res],J):- \+ verificar_se_pertence(Perm,J),
                                        permutacao_possivel_espaco(A, Esp, Espacos, _,Res,J).
                             

% ver se o elemento pertence a pelo menos um conjunto de listas
pertence_a_1_lista(El, Listas) :-member(Lista, Listas),
                    member(El, Lista),!.


% verificar_se_pertence([1,9],[[1,2,3],[9,8,7]]).
% ver se uma lista pertence a uma quantidade de listas
verificar_se_pertence([],[]).
verificar_se_pertence([A|Perms],[B|Espacos]):- pertence_a_1_lista(A,B),verificar_se_pertence(Perms,Espacos).

% faz a lista de lista com espacos possiveis
apanhar_lista([],[]).
apanhar_lista([A|Lista],Novo):- A= [_|Perms],append(Perms,Res,Novo),apanhar_lista(Lista,Res).

%-------------------------------------------------------------------------------
%       permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss)
% Espacos é uma lista de espaços,
% Perms_soma é uma listade listas tal como obtida pelo predicado permutacoes_soma_espacos,
% Esp é um espaço, significa que Perms_poss é uma lista de 2 elementos em que
% o primeiro é a lista de variáveis de Esp e
% o segundo é a lista ordenada de permutações possíveis para o espaço Esp.
%-------------------------------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, _, Esp,Perms_poss):- bagof(Aux,permutacao_possivel_espaco(Aux, Esp, Espacos, _),B),
                                                            lista_espaco(Esp,A),append([A],[B],Perms_poss),!.

%-------------------------------------------------------------------------------
%       permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos é uma lista de espaços,
% Perms_poss_esps é a lista de permutações possíveis
%-------------------------------------------------------------------------------

% needs optimization
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):- permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Espacos).
permutacoes_possiveis_espacos([], [],_). 
permutacoes_possiveis_espacos([Esp|Espacos], [Perms|Perms_poss_esps],Todos):- permutacoes_possiveis_espaco(Todos, _, Esp,Perms),
                                                                    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Todos).

%-------------------------------------------------------------------------------
%               numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms é uma lista de permutações,
% Numeros_comuns é uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contêm
% o número numero na posição pos.
%-------------------------------------------------------------------------------

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

% funcao auxiliar para obter o vetor comum de todas as listas
obter_vetor_comum([A],A).
obter_vetor_comum([A|B],L):-  mesmo_sitio(Res,A,L), obter_vetor_comum(B,Res),!.

% funcao auxiliar para ver se duas listas tem o elemntos no mesmo sitio,
% se nao tiverem e adicionado um 0
mesmo_sitio([],[],[]).

mesmo_sitio([B|L1],[B|L2],[B|Res]):-   mesmo_sitio(L1,L2,Res).

mesmo_sitio([B|L1],[D|L2],[0|Res]):- B \== D, mesmo_sitio(L1,L2,Res).

%-------------------------------------------------------------------------------
%               atribui_comuns(Perms_Possiveis)
% Perms_Possiveis é uma lista de permutações possíveis,
% actualiza esta lista atribuindo a cada espaço números comuns a
% todas as permutações possíveis para esse espaço
%-------------------------------------------------------------------------------

atribui_comuns([]).

atribui_comuns([A|Perms_Possiveis]):- nth1(1,A,D),nth1(2,A,L),
                                        numeros_comuns(L,Lista),
                                        Lista \== [],
                                        juntar(D,Lista),
                                        atribui_comuns(Perms_Possiveis).

 atribui_comuns([A|Perms_Possiveis]):- A = [_|_],atribui_comuns(Perms_Possiveis).
                                    
% funcao auxiliar que substitui valores
juntar(_,[]).

juntar(D,[A|Lista]):- nth1(Pos,D,Valor), A = (Pos,Valor), juntar(D,Lista).

%-------------------------------------------------------------------------------
%               retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis é uma lista de permutações possíveis,
% significa que Novas_Perms_Possiveis é
% o resultado de tirar permutações impossíveis de Perms_Possiveis
%-------------------------------------------------------------------------------

retira_impossiveis(Perms_Possiveis,Res):- obter_lista(Perms_Possiveis,L),retira_impossiveis_aux(L,Res),!.

retira_impossiveis_aux([],[]).

retira_impossiveis_aux([F|List],[M|Res]):- F = [A|B],
                verificar(A,Filtro),
                filtra(B,Filtro,J), M = [A|[J]],retira_impossiveis_aux(List,Res),!.

% funcao que obtem a lista de perms_possiveis para espacos
obter_lista([],[]).

obter_lista([A|Res],[L|Resto]):- A = [B|C], C = [D|_],L = [B|D],  obter_lista(Res,Resto),!.


% funcao que verificar se o elmento duma lista e variavel ou nao

verificar([],[]).
verificar([A|Resto],[0|Res]):- var(A), verificar(Resto,Res),!.

verificar([A|Resto],[A|Res]):- nonvar(A), verificar(Resto,Res),!.


% funcao que ve se uma permutacao pode esta de acordo com o filtro
filtra([],_,[]).

filtra([A|Lst_Perms],Filtro,[A|Res]):- possivel(A,Filtro),  filtra(Lst_Perms,Filtro,Res),!.
filtra([A|Lst_Perms],Filtro,Res):- \+ possivel(A,Filtro),  filtra(Lst_Perms,Filtro,Res),!.


%verifica se a permutacao e possivel
possivel([],[]).

possivel([_|Res],[B|C]):- B == 0, possivel(Res,C),!.
possivel([A|Res],[B|C]):- B == A, possivel(Res,C),!.

%-------------------------------------------------------------------------------
%              simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis é uma lista de permutações possíveis,
% Novas_Perms_Possiveis é o resultado de simplificar Perms_Possiveis.
% Para simplificar uma lista de permutações possíveis,
% deve aplicar-lhe os predicados atribui_comuns e retira_impossiveis,
% por esta ordem, até não haver mais alterações.
%-------------------------------------------------------------------------------