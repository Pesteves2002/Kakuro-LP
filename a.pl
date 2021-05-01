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


cria_espaco([A|Res],v,espaco(W,Res)):- A = [W|_].

cria_espaco([A|Res],h,espaco(W,Res)):- A = [_,W|_].


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

