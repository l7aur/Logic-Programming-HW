% The member1/2 predicate
% member1(X, L) returns true if X is in L
member1(X, [_|T]) :-
	member1(X, T).
member1(X, [X|_]).

% The append1/2 predicate
% append1(L1, L2) appends the second list to the end of the first list
append1([], L2, L2).
append1([H|T], L2, [H|TailR]) :-
	append1(T, L2, TailR).

% The delete1/3 predicate
% delete1(X, L, R) deletes once occurence of X from the list L and returns the result in R
delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]) :-
	delete1(X, T, R).
delete1(_, [], []).


% The delete_all/3 predicate
% delete_all(X, L, R) deletes all occurences of X from the list L and returns the result in R
delete_all(X, [X|T], R) :- 
	delete_all(X, T, R).
delete_all(X, [H|T], [H|R]) :- 
	delete_all(X, T, R).
delete_all(_, [], []).

% The add_first/3 predicate
% add_first(X, L, R) adds X to the beginning of L and returns the result in R
add_first(X, L, [X|L]).

% The append3/4 predicate
% append3(L1, L2, L3, R) appends the third list to the end of the second list and appends that to the end of the first list, returning the result in R
append3([H|T], L2, L3, [H|TailR]) :-
	append3(T, L2, L3, TailR).
append3([], [H|T], L3, [H|TailR]) :-
	append3([], T, L3, TailR).
append3([], [], L3, L3).
	
% The sum_bwd/2 predicate
% sum_bwd(L, R) returns the sum of all elements in L using backward recursion
sum_bwd([], 0).
sum_bwd([H|T], NewR) :-
	sum_bwd(T, R),
	NewR is R + H .
	
% The sum_fwd/2 predicate
% sum_fwd(L, R) returns the sum of all elements in L using forward recursion
% It acts as a wrapper to sum_fwdHelper/3 predicate that handles the computation using an accumulator
sum_fwd(L, R) :- sum_fwdHelper(L, 0, R).

	sum_fwdHelper([], Acc, Acc).
	sum_fwdHelper([H|T], Acc, R) :-
		NewAcc is Acc + H,
		sum_fwdHelper(T, NewAcc, R).

% The separate_parity/3 predicate
% separate_parity(L1, O, E) returns the odd numbers in L1 in O and the even numbers in L1 in E
separate_parity([H|T], [H|E], O) :-
	number(H),
	0 is H mod 2,
	separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]) :-
	number(H),
	1 is H mod 2,
	separate_parity(T, E, O).
separate_parity([H|T], E, O) :-
	not(number(H)),
	separate_parity(T, E, O).
separate_parity([], [], []).

% The remove_duplicates/2 predicate
% remove_duplicates(L, R) removes all consecutive duplicates from L and returns the result in R
% keep the last occurence
remove_duplicates([H|T], R) :-
	member1(H, T),
	!,
	remove_duplicates(T, R).
remove_duplicates([H|T], [H|R]) :-
	remove_duplicates(T, R).
remove_duplicates([], []).

% The replace_all/4 predicate
% replace_all(X, Y, L, R) replaces all the occurences of X in L with Y and returns the result in R
% bad usage of prunning, but works
replace_all(_, _, [], []).
replace_all(X, A, [X|T], [A|R]) :- 
	replace_all(X, A, T, R),
	!.
replace_all(X, A, [H|T], [H|R]) :- 
	replace_all(X, A, T, R),
	!.

% The replace_all1/4 predicate
% replace_all1(X, Y, L, R) replaces all the occurences of X in L with Y and returns the result in R
replace_all1(_, _, [], []).
replace_all1(X, A, [X|T], [A|R]) :-
	replace_all1(X, A, T, R).
replace_all1(X, A, [H|T], [H|R]) :-
	X \= H,
	replace_all1(X, A, T, R).
	
% The drop_k/3 predicate
% drop_k(L, K, R) drops all elements at the N*K-th position elements in L
drop_k(L, K, R) :-
	K > 0,
	drop_kHelper(L, K, 0, R).
	
	drop_kHelper([_|T], K, I, R) :-
		NewI is (I + 1) mod K,
		NewI = 0,
		drop_kHelper(T, K, NewI, R).
	drop_kHelper([H|T], K, I, [H|R]) :-
		NewI is (I + 1) mod K,
		NewI > 0,
		drop_kHelper(T, K, NewI, R).
	drop_kHelper([], _, _, []).

% The remove_consecutive_duplicates/2 predicate
% remove_consecutive_duplicates(L, R) maps a group of identical items into one item and returns the result in R
remove_consecutive_duplicates([], []).
remove_consecutive_duplicates([H], [H]).
remove_consecutive_duplicates([H, H|T], R) :-
	remove_consecutive_duplicates([H|T], R).
remove_consecutive_duplicates([H1, H2|T], [H1|R]) :-
	H1 \= H2,
	remove_consecutive_duplicates([H2|T], R).
	
% The pack_consecutive_duplicates/2 predicate
% pack_consecutive_duplicates(L, R) groups all identical consecutive elements into a set and returns the result in R
pack_consecutive_duplicates([], []).
pack_consecutive_duplicates([H], [[H]]).
pack_consecutive_duplicates([H, H|T], [[H|HPack]|R]) :-
	pack_consecutive_duplicates([H|T], [HPack|R]).
pack_consecutive_duplicates([H1, H2|T], [[H1]|R]) :-
	H1 \= H2,
	pack_consecutive_duplicates([H2|T], R).