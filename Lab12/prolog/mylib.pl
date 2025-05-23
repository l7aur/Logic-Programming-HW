extract([H|T], 0, H, T) :- !.
extract([H|T], K, X, [H|T1]) :- K1 is K-1, extract(T, K1, X, T1).

rnd_select(_, 0, []).
rnd_select(L, K, [X|R]) :- 
		length(L, Len),
		myrand(Len, Pos),
		extract(L, Pos, X, L1),
		K1 is K-1,
		rnd_select(L1, K1, R).
