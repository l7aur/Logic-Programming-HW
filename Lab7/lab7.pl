% ===========================================================================================================================   Predefined in the lab

tree1(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))). 
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))). 
ternary_tree(t(6, t(4, t(2, nil, nil, nil), nil, t(7, nil, nil, nil)), t(5, nil, nil, nil), t(9, t(3, nil, nil, nil), nil, nil))).

% The pretty_print/1 predicate
% pretty_print(T) prints the tree in a pretty format
pretty_print(T) :- 
    pretty_print(T, 0),
    !.
pretty_print(nil, _).
pretty_print(t(K, L, R), D) :-
    D1 is D + 1,
    pretty_print(L, D1),
    print_key(K, D),
    pretty_print(R, D1).

print_key(K, D) :-
    D > 0,
    !,
    D1 is D - 1,
    tab(8),
    print_key(K, D1).
print_key(K, _) :-
    write(K),
    nl.

delete_key(Key, t(Key, L, nil), L) :- 
    !. 
delete_key(Key, t(Key, nil, R), R) :- 
    !. 
delete_key(Key, t(Key, L, R), t(Pred, NL, R)) :- 
    !, 
    get_pred(L, Pred, NL). 
delete_key(Key, t(K, L, R), t(K, NL, R)) :- 
    Key<K, 
    !, 
    delete_key(Key, L, NL). 
delete_key(Key, t(K, L, R), t(K, L, NR)) :- 
    delete_key(Key, R, NR). 

% search for the predecessor node 
get_pred(t(Pred, L, nil), Pred, L) :-
    !. 
get_pred(t(Key, L, R), Pred, t(Key, L, NR)) :- 
    get_pred(R, Pred, NR). 

% ===========================================================================================================================   EXERCISE 1

% The ternary_preorder/2 predicate
% ternary_preorder(T, R) returns the list obtained by traversing the tree T in preorder
ternary_preorder(t(K, Ltree, Mtree, Rtree), R) :-
    ternary_preorder(Ltree, LL),
    ternary_preorder(Mtree, ML),
    ternary_preorder(Rtree, RL),
    append([K|LL], ML, PL),
    append(PL, RL, R).
ternary_preorder(nil, []).

% The ternary_inorder/2 predicate
% ternary_inorder(T, R) returns the list obtained by traversing the tree T in inorder
ternary_inorder(t(K, Ltree, Mtree, Rtree), R) :-
    ternary_inorder(Ltree, LL),
    ternary_inorder(Mtree, ML),
    ternary_inorder(Rtree, RL),
    append(LL, [K|ML], PL),
    append(PL, RL, R).
ternary_inorder(nil, []).

% The ternary_postorder/2 predicate
% ternary_postorder(T, R) returns the list obtained by traversing the tree T in postorder
ternary_postorder(t(K, Ltree, Mtree, Rtree), R) :-
    ternary_postorder(Ltree, LL),
    ternary_postorder(Mtree, ML),
    ternary_postorder(Rtree, RL),
    append(LL, ML, PL),
    append(PL, RL, PR),
    append(PR, [K], R).
ternary_postorder(nil, []).

% ===========================================================================================================================   EXERCISE 2

% The pretty_print_ternary/1 predicate
% pretty_print_ternary(T) prints the ternary tree T in a pretty format
pretty_print_ternary(T) :-
    pretty_print_ternary(T, 0).
pretty_print_ternary(t(K, L, M, R), D) :-
    print_key(K, D),
    NewD is D + 1,
    pretty_print_ternary(L, NewD),
    pretty_print_ternary(M, NewD),
    pretty_print_ternary(R, NewD).
pretty_print_ternary(nil, _).


% ===========================================================================================================================   EXERCISE 3

% The max/2 predicate
% max(L, M) returns the maximum in the list L
max([H|T], M) :-
    max(T, M),
    H < M,
    !.
max([H|_], H).

% The ternary_height/2 predicate
% ternary_height(T, H) computes the height of the ternary tree T and returns it in H
ternary_height(t(_, L, M, R), H) :-
    ternary_height(L, HL),
    ternary_height(M, HM),
    ternary_height(R, HR),
    NewHL is HL + 1,
    NewHM is HM + 1,
    NewHR is HR + 1,
    max([NewHL, NewHM, NewHR], H).
ternary_height(nil, 0).

% ===========================================================================================================================   EXERCISE 4

% The leaf_list/2 predicate
% leaf_list(T, L) collects all the leaves of the binary tree T in L
leaf_list(nil, []).
leaf_list(t(K, nil, nil), [K]) :-
    !.
leaf_list(t(_, L, R), Rez) :-
    leaf_list(L, Rez1),
    leaf_list(R, Rez2),
    append(Rez1, Rez2, Rez).

% ===========================================================================================================================   EXERCISE 5

% The internal_list/2 predicate
% internal_list(T, L) collects all the internal nodes of the binary tree T in L
internal_list(nil, []).
internal_list(t(_, nil, nil), []) :-
    !.
internal_list(t(K, L, R), Rez) :-
    internal_list(L, Rez1),
    internal_list(R, Rez2),
    append(Rez1, [K|Rez2], Rez).

% ===========================================================================================================================   EXERCISE 6

% The same_depth/3 predicate
% same_depth(T, D, L) collects all the nodes at depth D in a binary tree T and returns them in L
% Note: root is depth 1
same_depth(nil, _, []).
same_depth(t(K, _, _), 1, [K]) :- 
    !.
same_depth(t(_, Ltree, Rtree), D, R) :-
    NewD is D - 1,
    same_depth(Ltree, NewD, R1),
    same_depth(Rtree, NewD, R2),
    append(R1, R2, R).

% ===========================================================================================================================   EXERCISE 7

% The diam/2 predicate
% diam(T, D) computes the diameter of the tree T
diam(T, D) :-
    diam(T, _, D),
    !.
diam(nil, 0, 0).
diam(t(_, L, R), H, D) :-
    diam(L, LH, LD),
    diam(R, RH, RD),
    NewD is LH + RH + 1,
    NewLH is LH + 1,
    NewRH is RH + 1, 
    max([NewLH, NewRH], H),
    max([LD, RD, NewD], D).

% ===========================================================================================================================   EXERCISE 8

% Test data
my_tree1(t(3, t(5, nil, nil), t(10, nil, nil))). % true
my_tree2(t(3, t(5, t(10, nil, nil), t(7, nil, nil)), t(10, t(5, nil, nil), t(4, nil, nil)))). % true
my_tree3(t(3, t(5, t(10, nil, t(0, nil, nil)), t(7, nil, nil)), t(10, t(5, nil, t(-1, nil, t(x, nil, nil))), t(4, nil, nil)))). % false
my_tree4(nil). % true
my_tree5(t(2, nil, t(3, nil, nil))). % false
my_tree6(t(2, nil, nil)). % true
my_tree7(t(3, t(5, nil, nil), t(10, t(2, nil, nil), nil))). % false

% The symmetric/1 predicate
% symmetric(T) checks whether T has a symmetric structure
symmetric(T) :-
    symmetric(T, T).

symmetric(nil, nil) :-
    !. 
symmetric(t(_, nil, nil), t(_, nil, nil)) :-
    !.
symmetric(t(_, L1, nil), t(_, nil, L2)) :-
    not(atomic(L1)),
    not(atomic(L2)),
    symmetric(L1, L2). 
symmetric(t(_, nil, R1), t(_, R2, nil)) :-
    not(atomic(R1)),
    not(atomic(R2)),
    symmetric(R1, R2).
symmetric(t(_, L1, R1), t(_, R2, L2)) :-
    not(atomic(L1)),
    not(atomic(L2)),
    not(atomic(R1)),
    not(atomic(R2)),
    symmetric(L1, L2),
    symmetric(R1, R2).   

% ===========================================================================================================================   EXERCISE 9

% The delete_key_succ/3 predicate
% delete_key_succ(K, T, NewT) deletes the key K in T and returns the new tree in NewT
delete_key_succ(Key, t(Key, L, nil), L) :-
    !. 
delete_key_succ(Key, t(Key, nil, R), R) :-
    !. 
delete_key_succ(Key, t(Key, L, R), t(Succ, L, NR)) :- 
    !, 
    get_succ(R, Succ, NR). 
delete_key_succ(Key, t(K, L, R), t(K, NL, R)) :- 
    Key < K, 
    !, 
    delete_key_succ(Key, L, NL). 
delete_key_succ(Key, t(K ,L, R), t(K, L, NR)) :-
    delete_key_succ(Key, R, NR). 

get_succ(t(Succ, nil, R), Succ, R) :-
    !. 
get_succ(t(Key, L, R), Succ, t(Key, NL, R)) :-
    get_succ(L, Succ, NL).