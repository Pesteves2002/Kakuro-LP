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