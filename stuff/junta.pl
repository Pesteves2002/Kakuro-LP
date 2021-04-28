% junta(X, Y, Z) - > é o resultado de juntar X a Y
junta([], L, L).
% a junção da lista vazia a uma lista qualquer é a própria lista
junta([P | R], L1, [P | L2]) :- junta(R, L1, L2).
% sendo L2 a junção de R a L1,
% a junção de uma lista iniciada por P com resto R a L1 dá uma lista P | L2
