member1(X, [_|T]) :-
	member1(X, T).
member1(X, [X|_]).


append1([], L2, L2).
append1([H|T], L2, [H|TailR]) :-
	append1(T, L2, TailR).
	
delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]) :-
	delete1(X, T, R).
delete1(_, [], []).

delete_all(X, [X|T], R) :- delete_all(X, T, R).
delete_all(X, [H|T], [H|R]) :- delete_all(X, T, R).
delete_all(_, [], []).

add_first(X, L, [X|L]).

append3([H|T], L2, L3, [H|TailR]) :-
	append3(T, L2, L3, TailR).
append3([], [H|T], L3, [H|TailR]) :-
	append3([], T, L3, TailR).
append3([], [], L3, L3).
	
sum_bwd([], 0).
sum_bwd([H|T], NewR) :-
	sum_bwd(T, R),
	NewR is R + H .
	
sum_fwd(L, R) :- sum_fwdHelper(L, 0, R).

	sum_fwdHelper([], Acc, Acc).
	sum_fwdHelper([H|T], Acc, R) :-
		NewAcc is Acc + H,
		sum_fwdHelper(T, NewAcc, R).
		
separate_parity([], [], []).
separate_parity([H|T], [H|E], O) :-
	0 is H mod 2,
	separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]) :-
	1 is H mod 2,
	separate_parity(T, E, O).
	
% keep the 1st occurence
remove_duplicates([], []).
remove_duplicates([H|T], [H|R]) :-
	delete_all(H, T, NewR),
	remove_duplicates(NewR, R),
	!.
	
% keep the last occurence
remove_duplicates1([], []).
remove_duplicates1([H|T], R) :-
	member1(H, T),
	remove_duplicates1(T, R).
remove_duplicates1([H|T], [H|R]) :-
	not(member1(H, T)),
	remove_duplicates1(T, R).
	
replace_all(_, _, [], []).
replace_all(X, A, [X|T], [A|R]) :- 
	replace_all(X, A, T, R),
	!.
replace_all(X, A, [H|T], [H|R]) :- 
	replace_all(X, A, T, R),
	!.

replace_all1(_, _, [], []).
replace_all1(X, A, [X|T], [A|R]) :-
	replace_all1(X, A, T, R).
replace_all1(X, A, [H|T], [H|R]) :-
	not(X is H),
	replace_all1(X, A, T, R).
	
drop_k(L, K, R) :-
	drop_kHelper(L, K, 0, R).
	
	drop_kHelper([], _, _, []).
	drop_kHelper([_|T], K, I, R) :-
		NewI is (I + 1) mod K,
		NewI is 0,
		drop_kHelper(T, K, NewI, R).
	drop_kHelper([H|T], K, I, [H|R]) :-
		NewI is (I + 1) mod K,
		not(NewI is 0),
		drop_kHelper(T, K, NewI, R).

remove_consecutive_duplicates([], []).
remove_consecutive_duplicates([H], [H]).
remove_consecutive_duplicates([H, H|T], R) :-
	remove_consecutive_duplicates([H|T], R).
remove_consecutive_duplicates([H1, H2|T], [H1|R]) :-
	not(H1 is H2),
	remove_consecutive_duplicates([H2|T], R).
	
pack_consecutive_duplicates([], []).
pack_consecutive_duplicates([H], [[H]]).
pack_consecutive_duplicates([H, H|T], [[H|HPack]|R]) :-
	pack_consecutive_duplicates([H|T], [HPack|R]).
pack_consecutive_duplicates([H1, H2|T], [[H1]|R]) :-
	not(H1 is H2),
	pack_consecutive_duplicates([H2|T], R).