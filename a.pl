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

% se for lista e h
ola([A|B],h,[W|P]):- writeln(A),is_list(A),A = [W|_] ,writeln(W),!,ola(B,h,P).
% se for lista e v
ola([A|B],v,[Z|P]):- writeln(A),is_list(A),A = [_|W] ,W = [Z|_],writeln(Z),!,ola(B,v,P).
%se nao for lista
ola([A|B],H_V,[A|P]):- ola(B,H_V,P). 
% quando e fila vazia
ola([],_,[]).

% É é o elemento, Acumulador sao os elementos

espaco_fila(Fila, Esp, H_V):- espaco_fila_aux(Fila, Esp, H_V,[]).

espaco_fila_aux([],[],_,_).

espaco_fila_aux([A|Res],[A|Acc],H_V,[]):- is_list(A),espaco_fila_aux(Res,_,H_V,Acc).

espaco_fila_aux([A|Res],Esp,H_V,[A|Acc]):- espaco_fila_aux(Res,Esp,H_V,Acc).


