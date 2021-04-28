% inverte(L, LI) - LI é L invertida
inverte([], []).
% a inversão da lista vazia é ela própria
inverte([P | R], LI) :- inverte(R, RI), junta(RI, [P], LI).
% sendo RI a inversão de R, e LI a junção de RI a P,
% a inversão de P|R resultará em LI
