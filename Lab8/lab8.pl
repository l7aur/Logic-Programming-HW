% ================================================================================================================= PREDEFINED IN THE LAB
% The member_il/2 predicate
% member_il(X, L) checks if X is in the incomplete list L
member_il(_, L) :-
    var(L),
    !,
    fail.
member_il(X, [X|_]) :-
    !.
member_il(X, [_|T]) :-
    member_il(X, T).

% The insert_il1/2 predicate
% insert_il1(X, L) inserts an element at the end of the list if not already present
insert_il1(X, L) :-
    var(L),
    !,
    L = [X|_].
insert_il1(X, [X|_]) :-
    !.
insert_il1(X, [_|T]) :-
    insert_il1(X, T).

% The insert_il2/2 predicate
% insert_il2(X, L) is a refactoring of the insert_il1/2 predicate
insert_il2(X, [X|_]) :-
    !.
insert_il2(X, [_|T]) :-
    insert_il2(X, T).

% The delete_il/3 predicate
% delete_il(X, L, R) deleted first appearance X from L and returns the result in R
delete_il(_, L, L) :-
    var(L),
    !.
delete_il(X, [X|T], T) :-
    !.
delete_il(X, [H|T], [H|R]) :-
    delete_il(X, T, R).

% The search_it/2 predicate
% search(X, T) searches for X in the tree T
search_it(_, T) :-
    var(T),
    !,
    fail.
search_it(Key, t(Key, _, _)) :-
    !.
search_it(Key, t(K, L, _)) :-
    Key < K,
    !,
    search_it(Key, L).
search_it(Key, t(_, _, R)) :-
    search_it(Key, R).

% The insert_it/2 predicate
% insert_it(X, T) checks for the existance of X in T, if false inserts X in the tree T
insert_it(Key, t(Key, _, _)) :-
    !.
insert_it(Key, t(K, L, _)) :-
    Key < K,
    !,
    insert_it(Key, L).
insert_it(Key, t(_, _, R)) :-
    insert_it(Key, R).

% The delete_it/3 predicate
% delete_it(X, T, R) deletes X from the tree T and returns the new tree in R
delete_it(_, T, T) :-
    var(T),
    !.
delete_it(Key, t(Key, L, R), L) :-
    var(R),
    !.
delete_it(Key, t(Key, L, R), R) :-
    var(L),
    !.
delete_it(Key, t(Key, L, R), t(Pred, NL, R)) :-
    !,
    get_pred(L, Pred, NL).
delete_it(Key, t(K, L, R), t(K, NL, R)) :-
    Key < K,
    !,
    delete_it(Key, L, NL).
delete_it(Key, t(K, L, R), t(K, L, NR)) :-
    delete_it(Key, R, NR).

get_pred(t(Pred, L, R), Pred, L) :-
    var(R),
    !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)) :-
    get_pred(R, Pred, NR).

incomplete_tree(t(7, t(5, t(3, _, _), t(6, _, _)), t(11, _, _))). 
complete_tree(t(7, t(5, t(3, nil, nil), t(6, nil, nil)), t(11, nil, nil))).

% ================================================================================================================= EXERCISE 1
% convertIL2CL/2 predicate
% convertIL2CL(L1, L2) converts an incomplete list L1 to a complete list L2
convertIL2CL(L, []) :-
    var(L),
    !.
convertIL2CL([H|T], [H|R]) :-
    convertIL2CL(T, R).

convertCL2IL([X], [X|_]) :-
    !.
convertCL2IL([H|T], [H|R]) :-
    convertCL2IL(T, R).

% ================================================================================================================= EXERCISE 2
% The append_il/3 predicate
% append_il(L1, L2, L3) appends 2 incomplete lists and returns the result in L3
append_il(L1, L2, L2) :-
    var(L1),
    !.
append_il([H|T], L2, [H|R]) :-
    append_il(T, L2, R).

% ================================================================================================================= EXERCISE 3
% The reverse_il_fwd/2 predicate - forward recursion
% reverse_il_fwd(L, R) reverses an incomplete list L and returns the result in R
reverse_il_fwd(L, R) :-
    reverse_il_fwd(L, [], R).
reverse_il_fwd(L, A, R) :-
    var(L),
    !,
    append(A, L, R).
reverse_il_fwd([H|T], A, R) :-
    reverse_il_fwd(T, [H|A], R).

% The reverse_il_bwd/2 predicate - backwards recursion
% reverse_il_bwd(L, R) reverses an incomplete list L and returns the result in R
reverse_il_bwd(L, []) :-
    var(L),
    !.
reverse_il_bwd([H|T], NewR) :-
    reverse_il_bwd(T, R),
    append(R, [H|_], NewR).

% ================================================================================================================= EXERCISE 4
% flat_il/2 predicate
% flat_il(L, R) flattens the incomplete list L and returns the result in R

% ================================================================================================================= EXERCISE 5
% The convertIT2CT/2 predicate
% convertIT2CT(T, R) converts the incomplete tree T into a complete tree R

% The convertCT2IT/2 predicate
% convertCT2IT(T, R) converts the complete tree T into an incomplete tree R

% ================================================================================================================= EXERCISE 6
% The preorder_it/2 predicate
% preorder_it(T, R) performs a preorder traversal of the tree 
%   and collects the result into an incomplete list

% ================================================================================================================= EXERCISE 7
% The height_it/2 predicate
% height_it(T, H) computes the height of an incomplete tree

% ================================================================================================================= EXERCISE 8
% The diam_it(T, D) predicate
% diam_it(T, D) computes the diameter of an incomplete tree

% ================================================================================================================= EXERCISE 9
% The sub_il/2 predicate
% sub_il(L1, L2) checks if L1 is a sublist of L2

% ================================================================================================================= EXERCISE 10
% The append_il/2 predicate
% append(L, R) appends L to R and returns the result in R