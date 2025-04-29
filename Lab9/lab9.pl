% ====================================================================================================== PREDEFINED IN LAB

% The add_dl/5 predicate
% add_dl(X, LS, LE, RS, RE) adds x to the end of the difference list LS-LE
add_dl(X, LS, [X|RE], LS, RE).

% The append_dl/5 predicate
% append_dl(LS1, LE1, LS2, LE2, RS, RE) appends 2 difference lists
append_dl(LS1, Link, Link, LE2, LS1, LE2).

% The quicksort_dl/3 predicate
% quicksort_dl(L, S, E) sorts the list L
quicksort_dl([H|T], S, E) :-
    partition(H, T, Smaller, Larger),
    quicksort_dl(Smaller, S, [H|L]),
    quicksort_dl(Larger, L, E).
quicksort_dl([], L, L).

% The partition/4 predicate
% partition(P, L, Smaller, Larger) separates the list L into a smaller and a larger list with respect to P
partition(P, [H|T], [H|Smaller], Larger) :-
    P > H,
    !,
    partition(P, T, Smaller, Larger).
partition(P, [H|T], Smaller, [H|Larger]) :-
    partition(P, T, Smaller, Larger).
partition(_, [], [], []).

% The inorder_dl/3 predicate
% inorder_dl(T, S, E) performs an inorder traversal of a tree
inorder_dl(nil, L, L).
inorder_dl(t(K, LT, RT), LS, RE) :-
    inorder_dl(LT, LS, [K|LE]),
    inorder_dl(RT, LE, RE).

% The fib/2 predicate
% fib(N, F) computes the N'th Fibonacci number using memoisation
:-dynamic memo_fib/2.
fib(N, F) :-
    memo_fib(N, F),
    !.
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2,
    assertz(memo_fib(N, F)).
fib(0, 1).
fib(1, 1).

% Failure-driven loop
print_all :-
    memo_fib(N, F),
    write(N),
    write('-'),
    write(F),
    nl,
    fail.
print_all.

% The all_perm/2 predicate
% all_perm(L, R) generates all the permutations of L and collects them in R
all_perm(L, _) :-
    perm(L, L1),
    assertz(p(L1)),
    fail.
all_perm(_, R) :-
    collect_perms(R).

collect_perms([L1|R]) :-
    retract(p(L1)),
    !,
    collect_perms(R).
collect_perms([]).

perm(L, [H|R]) :-
    append(X, [H|Y], L),
    append(X, Y, L1),
    perm(L1, R).
perm([], []).

% Constants
complete_tree(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))). 
incomplete_tree(t(6, t(4,t(2,_,_),t(5,_,_)), t(9,t(7,_,_),_))). 

% ====================================================================================================== EXERCISE 1
% The convertCL2DL/3 predicate
% convertCL2DL(L, S, E) converts a complete list to a difference list
convertCL2DL([H|T], [H|S], E) :-
    convertCL2DL(T, S, E).
convertCL2DL([], E, E).

% The convertDL2CL/3 predicate
% convertDL2CL(S, E, L) converts a difference list to a complete list
convertDL2CL(E, E, []) :-
    var(E),
    !.
convertDL2CL([H|T], E, [H|L]) :-
    convertDL2CL(T, E, L).

% ====================================================================================================== EXERCISE 2
% The convertIL2DL/3 predicate
% convertIL2DL(L, S, E) converts an incomplete list to a difference list
convertIL2DL([H|_], E, E) :-
    var(H),
    !.
convertIL2DL([H|T], [H|S], E) :-
    convertIL2DL(T, S, E).

% The convertDL2IL/3 predicate
% convertDL2IL(S, E, L) converts a difference list to an incomplete list
convertDL2IL(E, E, _) :-
    var(E),
    !.
convertDL2IL([H|T], E, [H|L]) :-
    convertDL2IL(T, E, L).

% ====================================================================================================== EXERCISE 3
% The flat_dl/3 predicate
% flat_dl(L, RS, RE) flattens a deep list using difference lists
flat_dl([], E, E).
flat_dl([H|T], [H|S], E) :-
    atomic(H),
    !,
    flat_dl(T, S, E).
flat_dl([H|T], S, E) :-
    flat_dl(H, S, Link),
    flat_dl(T, Link, E).

% ====================================================================================================== EXERCISE 4


% ====================================================================================================== EXERCISE 5
% The preoreder_dl/3 predicate
% preorder_dl(T, S, E) computes a preorder traversal of the complete tree T
preorder_dl(nil, E, E).
preorder_dl(t(K, LT, RT), [K|S], E) :-
    preorder_dl(LT, S, Link),
    preorder_dl(RT, Link, E).

% The postorder_dl/3 predicate
% postorder_dl(T, S, E) computes a postorder traversal of the complete tree T
postorder_dl(nil, E, E).
postorder_dl(t(K, LT, RT), S, E) :-
    postorder_dl(LT, S, Link),
    postorder_dl(RT, Link, [K|E]).

% ====================================================================================================== EXERCISE 6
% The even_dl/3 predicate
% even_dl(T, S, E) collects all even nodes of a complete binary tree in a difference list
even_dl(nil, E, E) :-
    !.
even_dl(t(K, LT, RT), S, E) :-
    0 is K mod 2,
    !,
    even_dl(LT, S, [K|Link]),
    even_dl(RT, Link, E).
even_dl(t(_, LT, RT), S, E) :-
    even_dl(LT, S, Link),
    even_dl(RT, Link, E).

% ====================================================================================================== EXERCISE 7
% The between_dl/5 predicate
% between_dl(T, S, E, Min, Max) collects all nodes in an incomplete tree in a difference list
between_dl(Leaf, E, E, _, _) :-
    var(Leaf),
    !.
between_dl(t(K, LT, RT), S, E, Min, Max) :-
    K > Min,
    K < Max,
    !,
    between_dl(LT, S, [K|Link], Min, Max),
    between_dl(RT, Link, E, Min, Max).
between_dl(t(_, LT, RT), S, E, Min, Max) :-
    between_dl(LT, S, Link, Min, Max),
    between_dl(RT, Link, E, Min, Max).

% ====================================================================================================== EXERCISE 8
% The collect_depth_k/4 predicate
% collect_depth_k(T, D, S, E) collects all the nodes at a depth K in a difference list
collect_depth_k(Leaf, _, E, E) :-
    var(Leaf),
    !.
collect_depth_k(t(K, _, _), 1, [K|E], E) :-
    !.
collect_depth_k(t(_, LT, RT), D, S, E) :-
    D > 1,
    NewD is D - 1,
    collect_depth_k(LT, NewD, S, Link),
    collect_depth_k(RT, NewD, Link, E).