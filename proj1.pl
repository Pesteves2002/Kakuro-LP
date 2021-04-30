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


% caso de paragem
espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                           length(C,Tam),
                           Tam == 1,
                           cria_espaco(Fila,H_V,Esp),!.

%cria espaco
espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                           C = [_,Z|_],
                           split(Fila,Z,F,_),
                           cria_espaco(F,H_V,Esp).

%andar na lista
espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                              C = [_,Z|_],
                              split(Fila,Z,_,B),
                              Z \== [0,0],
                              espaco_fila([Z|B],Esp,H_V).

espaco_fila(Fila,Esp,H_V):- include(is_list,Fila,C),
                           C = [_,Z|_],
                           split(Fila,Z,_,B),
                           Z == [0,0],
                           espaco_fila(B,Esp,H_V).


split([X|T],E,[],T):- X == E.

split([X|T], E, [X|LL], LR) :-
    X \== E,
    split(T, E, LL, LR).