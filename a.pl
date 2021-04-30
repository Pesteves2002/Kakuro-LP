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

espaco_fila(Fila, Esp, H_V):- espaco_fila_aux(Fila,Esp,H_V,[]).

espaco_fila_aux([],_,_,[]).
% se a nao for uma lista
espaco_fila_aux([A|B],Perms,H_V,[A|Acumulador]):- \+ is_list(A), espaco_fila_aux(B,Perms,H_V,Acumulador). 

% espaco_fila_aux([A|B],Res,H_V,[]):-  is_list(A),cria_espaco([A|Acumulador],Res),espaco_fila_aux(B,_,H_V,Acumulador).

espaco_fila_aux([A|B],Perms,H_V,[]):- is_list(A),espaco_fila_aux(B,Perms,H_V,_). 


cria_espaco([A|Res],v,espaco(W,Res)):- A = [W|_].

cria_espaco([A|Res],h,espaco(W,Res)):- A = [_,W|_].


% caso de paragem
so_lista(L,A,H_V):- include(is_list,L,C),length(C,Tam), Tam == 1,cria_espaco(L,H_V,A),!.

%cria espaco
so_lista(L,A,H_V):- include(is_list,L,C),C = [_,Z|_],split(L,Z,F,_),cria_espaco(F,H_V,A).

%andar na lista
so_lista(L,A,H_V):- include(is_list,L,C),C = [_,Z|_],split(L,Z,_,B),Z \== [0,0],so_lista([Z|B],A,H_V).

so_lista(L,A,H_V):- include(is_list,L,C),C = [_,Z|_],split(L,Z,_,B),Z == [0,0],so_lista(B,A,H_V).


split([X|T],E,[],T):-    X == E.

split([X|T], E, [X|LL], LR) :-
    X \== E,
    split(T, E, LL, LR).