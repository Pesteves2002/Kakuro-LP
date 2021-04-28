% junta(X, Y, Z) - > � o resultado de juntar X a Y
junta([], L, L).
% a jun��o da lista vazia a uma lista qualquer � a pr�pria lista
junta([P | R], L1, [P | L2]) :- junta(R, L1, L2).
% sendo L2 a jun��o de R a L1,
% a jun��o de uma lista iniciada por P com resto R a L1 d� uma lista P | L2
