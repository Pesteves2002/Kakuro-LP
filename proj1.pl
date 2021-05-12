% Tomas Esteves - ist199341
:- [codigo_comum].

%-------------------------------------------------------------------------------
%                           Tads associadas ao espaco
%-------------------------------------------------------------------------------

%                           cria_espaco(L,H_V,Espaco)

% L eh a lista com a informacao para o espaco,
% H_V eh o o q distingue se queremos o espaco para a vertical ou horizontal,
% Espaco devolve os espaco no formato: espaco(soma, variaveis).
cria_espaco([A|Lista_Vars],v,espaco(V,Lista_Vars)):- A = [V|_].

cria_espaco([A|Lista_Vars],h,espaco(H,Lista_Vars)):- A = [_,H|_].

%                           lista_espaco(Espaco,Lista)

% Espaco eh um espaco(soma,variaveis)
% Lista eh variaveis
lista_espaco(espaco(_,Lista_Vars),Lista_Vars).

%                           numero_espaco(Espaco,Soma)

% Espaco eh um espaco(soma,variaveis)
% Soma eh soma
numero_espaco(espaco(Soma,_),Soma).

%-------------------------------------------------------------------------------
%                   combinacoes_soma(N, Els, Soma, Combs)
% N eh um inteiro,
% Els eh uma lista de inteiros,
% Soma eh um inteiro,
% Combs eh a lista ordenada cujos elementos sao as combinacoes N a N,
% dos elementos de Els cuja soma eh Soma.
%-------------------------------------------------------------------------------


combinacoes_soma(N, Els,Soma,Combs):- 
        bagof(Aux, (combinacao(N,Els,Aux), sumlist(Aux,Soma)), Combs),!.

combinacoes_soma(_, _,_, []).

%-------------------------------------------------------------------------------
%                   permutacoes_soma(N, Els, Soma, Perms)
% N eh um inteiro,
% Els eh uma lista de inteiros,
% Soma eh um inteiro, 
% Perms eh a lista ordenada cujos elementos sao
% as permutacoes das combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- 
        combinacoes_soma(N,Els,Soma,A),
        permutacoes(A,C),
        sort(C,Perms),!.

permutacoes([],[]).
permutacoes(Lista, Res) :- 
        findall(P,(member(Perm,Lista),permutation(P,Perm)), Res).

%-------------------------------------------------------------------------------
%                        espaco_fila(Fila, Esp, H_V)
% Fila eh uma fila (linha ou coluna) de um puzzle,
% H_V eh um dos atomos h ou v, fila horizontal ou vertical, respectivamente,
% Esp eh um espaco de Fila com formato(soma,variaveis).
%-------------------------------------------------------------------------------

% caso de paragem
espaco_fila(Fila,Esp,H_V):- 
        include(is_list,Fila,C),
        length(C,Tam),
        Tam == 1,
        Fila = [A|B],
        A \== [0,0],
        B \== [],
        cria_espaco(Fila,H_V,Esp),!.

%criar espaco
espaco_fila(Fila,Esp,H_V):- 
        include(is_list,Fila,C),
        C = [_,Z|_],
        separa_lista(Fila,Z,F,_),
        F = [A|B],
        A \== [0,0],
        B \== [],
        cria_espaco(F,H_V,Esp).

% adicionar a lista se Z for diferente de [0,0]
espaco_fila(Fila,Esp,H_V):- 
        include(is_list,Fila,C),
        C = [_,Z|_],
        separa_lista(Fila,Z,_,B),
        Z \== [0,0],
        espaco_fila([Z|B],Esp,H_V).

% passar a lista se Z for igual a [0,0]

espaco_fila(Fila,Esp,H_V):- 
        include(is_list,Fila,C),
        C = [_,Z|_],
        separa_lista(Fila,Z,_,B),
        Z == [0,0],
        espaco_fila(B,Esp,H_V).

% funcao auxilliar para separar uma lista
separa_lista([Novo|L],El,[],L):- Novo == El.

separa_lista([Novo|T], El, [Novo|Prim], Seg) :-
    Novo \== El,
    separa_lista(T, El, Prim, Seg).

%-------------------------------------------------------------------------------
%                    espacos_fila(H_V, Fila, Espacos)
% Fila eh uma fila (linha ou coluna) de uma grelha,
% H_V eh um dos atomos h ou v,
% Espacos eh a lista de todos os espacos de Fila, da esquerda para a direita.
%-------------------------------------------------------------------------------

espacos_fila(H_V,Fila,Esp):- 
        bagof(Aux,espaco_fila(Fila,Aux,H_V),Esp),!.

espacos_fila(_,_,[]).

%-------------------------------------------------------------------------------
%                    espacos_puzzle(Puzzle, Espacos)
% Puzzle eh um puzzle,
% Espacos eh a lista de espacos do Puzzle.
%-------------------------------------------------------------------------------

espacos_puzzle(Puzzle,Espacos):- 
        le_puzzle(Puzzle,Puzzle_Horizontal,h),
        mat_transposta(Puzzle,Transposta),
        le_puzzle(Transposta,Puzzle_Vertical,v),
        append(Puzzle_Horizontal,Puzzle_Vertical,Espacos).

%                  le_puzzle(Puzzle, Lista_Espacos, H_V)
% Puzzle eh um puzzle,
% Lista_Espacos eh a lista com o espacos do Puzzle,
% H_V eh um dos atomos h ou v.

% Caso de paragem
le_puzzle([],[],_).

% Caso de a lista ter espacos
le_puzzle([A|Res],Nova_Lista,H_V):- 
        espacos_fila(H_V,A,Lista_Espacos),
        Lista_Espacos \== [],
        append(Lista_Espacos,Antiga_Lista,Nova_Lista),
        le_puzzle(Res,Antiga_Lista,H_V),!.

% Caso de a lista nao ter espacos
le_puzzle([_|Resto_Puzzle],Lista_Espacos,H_V):-
        le_puzzle(Resto_Puzzle,Lista_Espacos,H_V),!.

%-------------------------------------------------------------------------------
%             espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Espacos eh uma lista de espacos,
% Esp eh um espaco,
% Esps_com eh a lista de espacos com variaveis em comum com Esp, sem Esp.
%-------------------------------------------------------------------------------

% Caso de paragem
espacos_com_posicoes_comuns([],_,[]).

% Caso de ser o proprio espaco
espacos_com_posicoes_comuns([Novo_Espaco|Res],Esp,Esps_com):- 
        Novo_Espaco == Esp,
        espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

% Caso de nao ser espaco em comum
espacos_com_posicoes_comuns([Novo_Espaco|Res],Esp,Esps_com):- 
        lista_espaco(Novo_Espaco,Lst_Novo_Espaco),
        lista_espaco(Esp,Lista_Espaco),
        \+ ver_comuns(Lst_Novo_Espaco,Lista_Espaco),!,
        espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

% Caso de ser espaco em comum
espacos_com_posicoes_comuns([Novo_Espaco|Res],Esp,[Novo_Espaco|Esps_com]):- 
        espacos_com_posicoes_comuns(Res,Esp,Esps_com),!.

% funcoes auxiliares

%                   ver_comuns(Lst_Novo_Espaco,Lst_Espaco)
% ver_comuns compara se as duas listas tem pelo menos um elemento em comum,
% Devolove true, se tiverem, se nao devolve false.

% Ver se o elemento pertence a Lst_Espaco
ver_comuns([El|_],Lst_Espaco):- membro(El,Lst_Espaco).

% Avanca na Lista
ver_comuns([_|B],Lst_Espaco):- ver_comuns(B,Lst_Espaco).

%                   membro(El, Lst_Espaco)
% funcao que se comporta de maneira semelhante ao member,
% no entanto nao unifica variaveis.

membro(El,[Prim|_]):- El == Prim,!.
membro(El,[_|Res]):- membro(El,Res).

%-------------------------------------------------------------------------------
%                   permutacoes_soma_espacos(Espacos, Perms_soma)
% Espacos eh uma lista de espacos, 
% Perms_soma eh a lista de listas de 2 elementos,
% 1o elemento eh um espaco de Espacos,  
% 2o eh a lista ordenada de permutacoes cuja soma eh igual a soma do espaco.
%-------------------------------------------------------------------------------

% Caso de paragem 
permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([Espaco|Res_Espacos],[Nova_Perm|Res_Perms]):- 
        lista_espaco(Espaco,Lst_Espaco),
        length(Lst_Espaco,N),
        numero_espaco(Espaco,Soma),
        permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
        append([Espaco],[Perms],Nova_Perm),
        permutacoes_soma_espacos(Res_Espacos,Res_Perms).

%-------------------------------------------------------------------------------
%             permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perm e uma permutacao,
% Esp e um espaco,
% Espacos e uma lista de espacos, 
% Perms_soma e uma lista de listas,
% Perm e uma permutacao possivel para o espaco Esp.
%-------------------------------------------------------------------------------

permutacao_possivel_espaco(Perm, Esp, Espacos, _):- 
        permutacoes_soma_espacos([Esp],Perm_Espaco),
        Perm_Espaco = [[_,Lista_Perm_Espaco]|_],
        espacos_com_posicoes_comuns(Espacos, Esp, Lista_Espacos_Comuns),
        permutacoes_soma_espacos(Lista_Espacos_Comuns,Lista_Soma_Comuns),
        juntar_permutacoes(Lista_Soma_Comuns,Lista_Perm_Possiveis),
        permutacao_possivel_espaco_aux(Perm, Esp,
        Espacos, Lista_Perm_Espaco,Lista_Perm_Possiveis).

% permutacao_possivel_espaco_aux(Perm, Esp,
%       Espacos, Lista_Perm_Espaco,Lista_Perm_Possiveis)

% Para cada elemento da Lista_Perm_Espaco, 
% verifica se encontra na Lista_Perm_Possiveis.

% Caso de a permutacao ser possivel
permutacao_possivel_espaco_aux(Perm, _, _, [Perm|_],Lista_Perm_Possiveis):- 
        verifica_pertence_a_1_elemento(Perm,Lista_Perm_Possiveis).

% Avancar na lista
permutacao_possivel_espaco_aux(Perm, Esp,
        Espacos, [_|Res],Lista_Perm_Possiveis):- 
        permutacao_possivel_espaco_aux(Perm, Esp,
        Espacos, Res,Lista_Perm_Possiveis).


% ver se o elemento pertence a pelo menos um conjunto de listas
pertence_a_1_lista(El, Listas) :-
        member(Lista, Listas),
        member(El, Lista),!.

% ver se uma lista pertence a uma quantidade de listas
verifica_pertence_a_1_elemento(_,[]).

verifica_pertence_a_1_elemento([A|Perms],[B|Espacos]):-
        pertence_a_1_lista(A,B),
        verifica_pertence_a_1_elemento(Perms,Espacos).

%           juntar_permutacoes(Lista_Espacos_Comuns,Lista_Perm_Possiveis)
% Lista_Espacos_Comuns eh uma lista com as permutacoes de cada espaco,
% Lista_Perm_Possiveis eh uma lista com apenas as permutacoes de cada espaco.

juntar_permutacoes([],[]).

juntar_permutacoes([Espaco|Lista_Espacos_Comuns],Nova_Lista_Perm_Possiveis):- 
        Espaco = [_|Perms],
        append(Perms,Lista_Perm_Possiveis,Nova_Lista_Perm_Possiveis),
        juntar_permutacoes(Lista_Espacos_Comuns,Lista_Perm_Possiveis).

%-------------------------------------------------------------------------------
%         permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss)
% Espacos eh uma lista de espacos,
% Perms_soma eh uma lista de listas,
% Esp eh um espaco,
% Perms_poss eh uma lista de 2 elementos em que o
% primeiro eh a lista de variaveis de Esp e
% o segundo eh a lista ordenada de permutacoes possiveis para o espaco Esp.
%-------------------------------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, _, Esp,[Lista|[Perms]]):- 
        bagof(Aux,permutacao_possivel_espaco(Aux, Esp, Espacos, _),Perms),
        lista_espaco(Esp,Lista).

%-------------------------------------------------------------------------------
%       permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos eh uma lista de espacos,
% Perms_poss_esps eh a lista de permutacoes possiveis.
%-------------------------------------------------------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):- 
        permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Espacos).

% Caso de paragem
permutacoes_possiveis_espacos([], [],_). 

permutacoes_possiveis_espacos([Esp|Espacos], [Perms|Perms_poss_esps],Todos):- 
        permutacoes_possiveis_espaco(Todos, _, Esp,Perms),
        permutacoes_possiveis_espacos(Espacos, Perms_poss_esps,Todos).

%-------------------------------------------------------------------------------
%               numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms eh uma lista de permutacoes,
% Numeros_comuns eh uma lista de pares (pos, numero),
% todas as listas de Lst_Perms contem o numero numero na posicao pos.
%-------------------------------------------------------------------------------

numeros_comuns(L,Lst_comuns):- 
        nth1(1,L,Lista_Referencia),
        length(Lista_Referencia,Tamanho),
        findall(Aux,(between(1,Tamanho,Posicao_Lista),
        nth1(Posicao_Lista,Lista_Referencia,Valor),
        compara(L,Posicao_Lista,Valor),
        Aux = (Posicao_Lista,Valor)),Lst_comuns).


%                     compara (L,Posicao_Lista,Valor)
% Verifica se os elementos duma dada posicao sao todos iguais a Valor
% Retorna true no caso de serem todos iguais ou false.

compara([],_,_).

compara([A|B],Posicao_Lista,Valor):- 
        nth1(Posicao_Lista,A,Valor),
        compara(B,Posicao_Lista,Valor).

%-------------------------------------------------------------------------------
%               atribui_comuns(Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis,
% actualiza esta lista atribuindo a cada espaco numeros comuns,
% a todas as permutacoes possiveis para esse espaco.
%-------------------------------------------------------------------------------


atribui_comuns([]).

% Caso de haver numeros comuns
atribui_comuns([El|Perms_Possiveis]):- 
        nth1(1,El,Lista_Vars),
        nth1(2,El,Lista_Perms),
        numeros_comuns(Lista_Perms,Perms_Possivel),
        Perms_Possivel \== [],
        juntar(Lista_Vars,Perms_Possivel),
        atribui_comuns(Perms_Possiveis),!.

% Caso de nao haver numeros comuns
atribui_comuns([_|Perms_Possiveis]):- atribui_comuns(Perms_Possiveis),!.
                                    
%                   juntar(Lista_Vars,Perms_Possivel)
% Atribui o valor da permutacao ao espaco

juntar(_,[]).

juntar(Lista_Vars,[Perm|Perms_Possivel]):- 
        nth1(Pos,Lista_Vars,Valor),
        Perm = (Pos,Valor),
        juntar(Lista_Vars,Perms_Possivel).

%-------------------------------------------------------------------------------
%          retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis,
% Novas_Perms_Possiveis eh o resultado de
% tirar permutacoes impossiveis de Perms_Possiveis.
%-------------------------------------------------------------------------------

retira_impossiveis(Perms_Possiveis,Novas_Perms_Possiveis):- 
        obter_lista(Perms_Possiveis,Lst_Perms_Possiveis),
        retira_impossiveis_aux(Lst_Perms_Possiveis,Novas_Perms_Possiveis),!.

retira_impossiveis_aux([],[]).

retira_impossiveis_aux([Nova_Lista|Lst_Perms_Possiveis],
        [Nova_Perm|Novas_Perms_Possiveis]):- 
        Nova_Lista = [A|B],
        verificar(A,Filtro),
        filtra(B,Filtro,J),
        Nova_Perm = [A|[J]],
        retira_impossiveis_aux(Lst_Perms_Possiveis,Novas_Perms_Possiveis),!.

%            obter_lista(Perms_Possiveis,Lst_Perms_Possiveis)
% obtem a lista de permutacaoes
obter_lista([],[]).

obter_lista([A|Res],[L|Resto]):- 
        A = [B|C],
        C = [D|_],
        L = [B|D],
        obter_lista(Res,Resto),!.


%                   verificar(A,Filtro)

verificar([],[]).

% Se for uma variavel adiciona um zero
verificar([A|Resto],[0|Res]):- var(A), verificar(Resto,Res),!.

% Se nao for uma variavel adiciona um o inteiro
verificar([A|Resto],[A|Res]):- nonvar(A), verificar(Resto,Res),!.


% funcao que ve se uma permutacao pode esta de acordo com o filtro
filtra([],_,[]).

filtra([A|Lst_Perms],Filtro,[A|Res]):- 
        possivel(A,Filtro),
        filtra(Lst_Perms,Filtro,Res),!.

filtra([_|Lst_Perms],Filtro,Res):- filtra(Lst_Perms,Filtro,Res),!.


%verifica se a permutacao e possivel
possivel([],[]).

possivel([_|Res],[B|C]):- B == 0, possivel(Res,C),!.
possivel([A|Res],[B|C]):- B == A, possivel(Res,C),!.

%-------------------------------------------------------------------------------
%              simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis,
% Novas_Perms_Possiveis eh o resultado de simplificar Perms_Possiveis.
%-------------------------------------------------------------------------------

simplifica(Perms_Possiveis, Perms_Possiveis):- 
        atribui_comuns(Perms_Possiveis),
        retira_impossiveis(Perms_Possiveis,Res),
        Res == Perms_Possiveis,!.

simplifica(Perms, Novas) :-
        atribui_comuns(Perms),
        retira_impossiveis(Perms, Res),
        simplifica(Res, Novas),!.

%-------------------------------------------------------------------------------
%                       inicializa(Puzzle, Perms_Possiveis)
% Puzzle eh um puzzle,
% Perms_Possiveis eh a lista de permutacoes possiveis simplificada para Puzzle.
%-------------------------------------------------------------------------------

inicializa(Puzzle, Res):- 
        espacos_puzzle(Puzzle, Espacos),
        permutacoes_possiveis_espacos(Espacos, Perms_Possiveis),
        simplifica(Perms_Possiveis,Res).

%-------------------------------------------------------------------------------
%               escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Perms_Possiveis e uma lista de permutacoes possiveis,
% Escolha e o elemento de Perms_Possiveis.
% Se todos os espacos em Perms_Possiveis tiverem associadas listas de
% permutacoes unitarias, o predicado deve devolver "falso".
%-------------------------------------------------------------------------------

escolhe_menos_alternativas(Perms_Possiveis, Escolha):- 
        escolher(Perms_Possiveis,[Escolha|_],_).


%               escolher(Perms_Possiveis,Escolha,Valor)
% eh dado o valor de 99999 para valor inicial de Valor
% a funcao verifica qual o menor espaco que tem valor maior que 1.

% Caso inicial
escolher([],[],99999).

% Caso de ter tamanho 1, ignora
escolher([El|L],Res,Antigo):- 
        ler_tamanho(El,Tam),
        Tam == 1,!,
        escolher(L,Res,Antigo).

% Caso de ter tamanho menor que o antigo
escolher([El|L],[El|Res],Tam):- 
        ler_tamanho(El,Tam),
        escolher(L,Res,Antigo),
        Antigo >= Tam,!.

% Caso de ter tamanho maior, ignora
escolher([El|L],Res,Antigo):- 
        ler_tamanho(El,Tam),
        escolher(L,Res,Antigo),
        Antigo < Tam,!.


ler_tamanho(L,Res):- 
        L = [_,Lista],
        length(Lista,Res).

%-------------------------------------------------------------------------------
%        experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis,
% Escolha eh o espaco que vai ser experimentada a mudanca
% Novas_Perms_Possiveis eh o resultado de experimentar a permutacao 
%-------------------------------------------------------------------------------

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis):- 
        Escolha = [Esp, Lst_Perms],
        member(Perm,Lst_Perms),
        Esp = Perm,
        maplist(igual_escolha(Escolha),Perms_Possiveis,Novas_Perms_Possiveis).

% avanca na lista
igual_escolha(Escolha,E,E):- 
        Escolha \== E,!.

% substitui na lista
igual_escolha(Escolha,E,A):- 
        Escolha == E,
        Escolha = [Esp|_] ,
        A = [Esp,[Esp]],!.

%-------------------------------------------------------------------------------
%             resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis,
% Novas_Perms_Possiveis e o resultado de aplicar o algoritmo a Perms_Possiveis.
%-------------------------------------------------------------------------------

resolve_aux(Perms_poss, Novas_Perms_Possiveis):- 
        escolhe_menos_alternativas(Perms_poss, Escolha),!,
        experimenta_perm(Escolha, Perms_poss, A),
        simplifica(A, B),
        resolve_aux(B,Novas_Perms_Possiveis),!.

% Caso de paragem
resolve_aux(Perms_Possiveis, A):- !,
        simplifica(Perms_Possiveis,A),!.

%-------------------------------------------------------------------------------
%                               resolve(Puz)
% Puz e um puzzle, 
% resolve o Puzzle.
%-------------------------------------------------------------------------------

resolve(Puz):- inicializa(Puz, A), resolve_aux(A, _).

