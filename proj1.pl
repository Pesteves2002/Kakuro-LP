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