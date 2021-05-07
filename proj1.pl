% Tomas Esteves - ist199341
:- [codigo_comum].

%-------------------------------------------------------------------------------
%                   combinacoes_soma(N, Els, Soma, Combs)
% N e um inteiro, Els e uma lista de inteiros, e Soma e um inteiro,
% significa que Combs e a lista ordenada cujos elementos sao as combinacoes N a N,
% dos elementos de Els cuja soma e Soma .
%-------------------------------------------------------------------------------


combinacoes_soma(N, Els,Soma,Combs):- bagof(L, (combinacao(N,Els,L), sumlist(L,Soma)), Combs),!.
combinacoes_soma(_, _,_, []).

%-------------------------------------------------------------------------------
%                   permutacoes_soma(N, Els, Soma, Perms),
% em que N e um inteiro, Els e uma lista de inteiros, e Soma e um inteiro, 
% significa que Perms e a lista ordenada cujos elementos sao as permutacoes das combinacoes N a N, 
% dos elementos de Els cuja soma eSoma .
%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- combinacoes_soma(N,Els,Soma,A),
                                        permutacoes(A,C),sort(C,Perms),!.

permutacoes([],[]).
permutacoes(Lista, Res) :- findall(P,(member(Perm,Lista),permutation(P,Perm)), Res).

%-------------------------------------------------------------------------------
%                      espaco_fila(Fila, Esp, H_V),
% em que Fila e uma fila (linha ou coluna) de um puzzle e H_V e um dos atomos h ou v,
% conforme se trate de uma fila horizontal ou vertical, respectivamente,
% significa que Esp e um espaco de Fila, tal como descrito na Seccao 2.1, no passo 1.
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
%  Fila e uma fila (linha ou coluna) de uma grelha e
%  H_V e um dos atomos h ou v, significa que Espacos e
% a lista de todos os espacos de Fila, da esquerda para a direita.
%-------------------------------------------------------------------------------

espacos_fila(H_V,Fila,Esp):- bagof(Aux,espaco_fila(Fila,Aux,H_V),Esp),!.
espacos_fila(_,_,[]).

%-------------------------------------------------------------------------------
%                       espacos_puzzle(Puzzle, Espacos)
% Puzzle e um puzzle, significa que Espacos e a lista de espacos de Puzzle,
% tal como descrito na Seccao 2.1, no passo 1.
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
% Espacos e uma lista de espacos e Esp e um espaco,
% significa que Esps_com e a lista de espacos com variaveis em comum com Esp, exceptuando Esp.
% Os espacos em Esps_com devem aparecer pela mesma ordem que aparecem em Espacos.
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
% Espacos e uma lista de espacos, 
% significa que Perms_soma e a lista de listas de 2 elementos,
% em que o 1o elemento e um espaco de Espacos e 
% o 2o e a lista ordenada de permutacoes cuja soma e igual a soma do espaco
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
% Perm e uma permutacao, Esp e um espaco, Espacos e uma lista de espacos, e
% Perms_soma e uma lista de listas tal como obtida pelo predicado anterior,
% significa que Perm e uma permutacao possivel para o espaco Esp.
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
% Espacos e uma lista de espacos,
% Perms_soma e uma listade listas tal como obtida pelo predicado permutacoes_soma_espacos,
% Esp e um espaco, significa que Perms_poss e uma lista de 2 elementos em que
% o primeiro e a lista de variaveis de Esp e
% o segundo e a lista ordenada de permutacoes possiveis para o espaco Esp.
%-------------------------------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, _, Esp,Perms_poss):- bagof(Aux,permutacao_possivel_espaco(Aux, Esp, Espacos, _),B),
                                                            lista_espaco(Esp,A),append([A],[B],Perms_poss),!.

%-------------------------------------------------------------------------------
%       permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos e uma lista de espacos,
% Perms_poss_esps e a lista de permutacoes possiveis
%-------------------------------------------------------------------------------

% needs optimization
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):- permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Espacos).
permutacoes_possiveis_espacos([], [],_). 
permutacoes_possiveis_espacos([Esp|Espacos], [Perms|Perms_poss_esps],Todos):- permutacoes_possiveis_espaco(Todos, _, Esp,Perms),
                                                                    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Todos).

%-------------------------------------------------------------------------------
%               numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms e uma lista de permutacoes,
% Numeros_comuns e uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contem
% o numero numero na posicao pos.
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
% Perms_Possiveis e uma lista de permutacoes possiveis,
% actualiza esta lista atribuindo a cada espaco numeros comuns a
% todas as permutacoes possiveis para esse espaco
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
% Perms_Possiveis e uma lista de permutacoes possiveis,
% significa que Novas_Perms_Possiveis e
% o resultado de tirar permutacoes impossiveis de Perms_Possiveis
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
% Perms_Possiveis e uma lista de permutacoes possiveis,
% Novas_Perms_Possiveis e o resultado de simplificar Perms_Possiveis.
% Para simplificar uma lista de permutacoes possiveis,
% deve aplicar-lhe os predicados atribui_comuns e retira_impossiveis,
% por esta ordem, ate nao haver mais alteracoes.
%-------------------------------------------------------------------------------

simplifica(Perms_Possiveis, Perms_Possiveis):- atribui_comuns(Perms_Possiveis),
                                             retira_impossiveis(Perms_Possiveis,Res),
                                              Res == Perms_Possiveis,!.

simplifica(Perms, Novas) :-
                        atribui_comuns(Perms),
                        retira_impossiveis(Perms, Res),
                        simplifica(Res, Novas).

%-------------------------------------------------------------------------------
%                       inicializa(Puzzle, Perms_Possiveis)
% Puzzle e um puzzle,
% Perms_Possiveis e a lista de permutacoes possiveis simplificada para Puzzle.
%-------------------------------------------------------------------------------

inicializa(Puzzle, Res):- espacos_puzzle(Puzzle, Espacos),
                        permutacoes_possiveis_espacos(Espacos, Perms_Possiveis),
                        simplifica(Perms_Possiveis,Res).

%-------------------------------------------------------------------------------
%               escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Perms_Possiveis e uma lista de permutacoes possiveis,
% Escolha e o elemento de Perms_Possiveis.
% Se todos os espacos em Perms_Possiveis tiverem associadas listas de
% permutacoes unitarias, o predicado deve devolver "falso".
%-------------------------------------------------------------------------------

% needs improvement

escolhe_menos_alternativas(Perms_Possiveis, Escolha):- escolher(Perms_Possiveis,[Escolha|_],_).

ler_tamanho(L,Res):- L = [_,B],length(B,Res).

escolher([],[],99999).
escolher([A|L],Res,Antigo):- ler_tamanho(A,Tam),Tam == 1,!, escolher(L,Res,Antigo).
escolher([A|L],[A|Res],Tam):- ler_tamanho(A,Tam),  escolher(L,Res,Antigo),Antigo >= Tam,!.
escolher([A|L],Res,Antigo):- ler_tamanho(A,Tam),  escolher(L,Res,Antigo),Antigo < Tam,!.

%-------------------------------------------------------------------------------
%        experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis,
% Escolha e um dos seus elementos (escolhido pelo predicado anterior),
% Passos:
% 1. Sendo Esp e Lst_Perms o espaco e a lista de permutacoes de Escolha, respectivamente,
% escolhe uma permutacao de Lst_Perms, Perm.
% 2. Unifica Esp com Perm.
% 3. Novas_Perms_Possiveis e o resultado de substituir, em Perms_Possiveis,
% o elemento Escolha pelo elemento [Esp, [Perm]].
%-------------------------------------------------------------------------------

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis):- Escolha = [Esp, Lst_Perms],
                                                            member(Perm,Lst_Perms),
                                                            Esp = Perm,
                                                            maplist(igual_escolha(Escolha),Perms_Possiveis,Novas_Perms_Possiveis),!.

igual_escolha(Escolha,E,A):- Escolha == E, Escolha = [Esp|_] ,A = [Esp,[Esp]],!.

igual_escolha(Escolha,E,E):- Escolha \== E.

%-------------------------------------------------------------------------------
%             resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis,
% Novas_Perms_Possiveis e o resultado de aplicar o algoritmo a Perms_Possiveis.
%-------------------------------------------------------------------------------