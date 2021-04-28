% inverte(L, LI) - LI � L invertida
inverte([], []).
% a invers�o da lista vazia � ela pr�pria
inverte([P | R], LI) :- inverte(R, RI), junta(RI, [P], LI).
% sendo RI a invers�o de R, e LI a jun��o de RI a P,
% a invers�o de P|R resultar� em LI
