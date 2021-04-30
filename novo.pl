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

% ola([[1,2],3,4,[5,6],7],h,L).
ola([A|B],h,[A|P]):- is_list(A),!,ola(B,h,P).
% se for lista e h
% se for lista e v
ola([A|B],v,[Z|P]):- is_list(A),A = [_|W] ,W = [Z|_],!,ola(B,v,P).
%se nao for lista
ola([A|B],H_V,[A|P]):- ola(B,H_V,P). 
% quando e fila vazia
ola([],_,[]).

% É é o elemento, Acumulador sao os elementos

espaco_fila(Fila, Esp, _):- espaco_fila_aux(Fila,Esp,_,[]).

espaco_fila_aux([],Esp,_,Acumulador):- cria_espaco(Acumulador,Esp).

% se a nao for uma lista
espaco_fila_aux([A|B],_,H_V,Acumulador):- \+ is_list(A), espaco_fila_aux(B,_,H_V,[A|Acumulador]).

% cria o espaco
espaco_fila_aux([A|_],Esp,_,Acumulador):- is_list(A), cria_espaco([A|Acumulador],Esp).

espaco_fila_aux([A|B],Esp,H_V,[]):- is_list(A), espaco_fila_aux(B,Esp,H_V,_).


%funcao cria espaco
cria_espaco([A|Res],espaco(W,Res)):- A = [W|_].