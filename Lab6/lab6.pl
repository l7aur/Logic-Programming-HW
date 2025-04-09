% ================================================================================================ PREDEFINED IN THE LAB
% The max/3 predicate
% max(A, B, R) returns the maximum of A and B in R
max(A, B, A) :- 
    A > B,
    !.
max(_, B, B).

% The depth/2 predicate
% depth(L, R) returns the maximum depth of the deep list L in R
depth([], 1) :-
    !.
depth(X, 0) :-
    atomic(X).
depth([H|T], R) :-
    atomic(H),
    !,
    depth(T, R).
depth([H|T], R) :-
    depth(H, R1),
    depth(T, R2),
    NewR1 is R1 + 1,
    max(NewR1, R2, R).

% The flatten/2 predicate
% flatten(L, R) flattens the deep list L and returns the result in R
flatten([], []).
flatten([H|T], R) :-
    atomic(H),
    !,
    flatten(T, R).
flatten([H|T], R) :-
    flatten(H, R1),
    flatten(T, R2),
    append(R1, R2, R).

% The skip/2 predicate
% skip(L, R) skips all atomic elements in the list
skip([],[]). 
skip([H|T],R) :- 
    atomic(H),
    !,
    skip(T,R). 
skip([H|T],[H|R]) :- 
    skip(T,R). 

% The heads1/2 predicate
% heads1(L, R) returns a list of all the heads in the components in the deep list L
% Variant 1
heads1([],[]). 
heads1([H|T],[H|R]) :- 
    atomic(H),
    !,
    skip(T,T1), 
    heads1(T1,R). 
heads1([H|T],R) :- 
    heads1(H,R1), 
    heads1(T,R2),
    append(R1,R2,R). 

% The heads2/2 predicate
% Variant 2
% heads(L, R) returns a list formed from all the heads of the components in the deep list L
heads2(L, R) :-
    heads2(L, R, 1).
heads2([], [], _).
heads2([H|T], [H|R], 1) :-
    atomic(H),
    !,
    heads2(T, R, 0).
heads2([H|T], R, 0) :-
    atomic(H),
    !,
    heads2(T, R, 0).
heads2([H|T], R, F) :-
    heads2(H, R1, 1),
    heads2(T, R2, F),
    append(R1, R2, R).

% The member1/2 predicate
% member1(X, L) checks if X is in L
% Note: Works only for atomic elements
member1(X, L):- 
    flatten(L,L1), 
    member(X,L1).

% The member2/2 predicate
% member2(L, DL) checks if L is insinde of DL
member2(H, [H|_]). 
member2(X, [H|_]) :- 
    member2(X,H),
    !. 
member2(X, [_|T]) :- 
    member2(X,T),
    !. 

% ================================================================================================ EXERCISE 1

% The count_atomic/2 predicate
% count_atomic(L, R) counts the number of atomic elements in L
count_atomic(L, R) :-
    count_atomic(L, 0, R).
count_atomic([], A, A).
count_atomic([H|T], Acc, R) :-
    atomic(H),
    !,
    NewAcc is Acc + 1,
    count_atomic(T, NewAcc, R).
count_atomic([H|T], Acc, Acc2) :-
    count_atomic(H, Acc, Acc1),
    count_atomic(T, Acc1, Acc2).

% ================================================================================================ EXERCISE 2

% The sum_atomic/2 predicate
% sum_atomic(L, R) sums all the elements of the deep list l
sum_atomic(L, R) :-
    sum_atomic(L, 0, R).
sum_atomic([], A, A).
sum_atomic([H|T], A, R) :-
    atomic(H),
    number(H),
    !,
    NewA is A + H,
    sum_atomic(T, NewA, R).
sum_atomic([H|T], A, R) :-
    atomic(H),
    !,
    char_code(H, NewH),
    NewA is A + NewH,
    sum_atomic(T, NewA, R).
sum_atomic([H|T], A, A2) :-
    sum_atomic(H, A, A1),
    sum_atomic(T, A1, A2).

% ================================================================================================ EXERCISE 3

% The replace/4 predicate
% replace(X, Y, L, R)  replaces the occurence of X (atomic) with Y (atomic) in the deep list L at any depth
replace(_, _, [], []) :- !.
replace(X, Y, [X|T], [Y|R]) :-
    !,
    replace(X, Y, T, R).
replace(X, Y, [H|T], [H|R]) :-
    atomic(H),
    !,
    replace(X, Y, T, R).
replace(X, Y, [H|T], [R1|R2]) :-
    replace(X, Y, H, R1),
    replace(X, Y, T, R2).

% ================================================================================================ EXERCISE 4

% The lasts/2 predicate
% lasts(L, R) extracts the last position in each nested list in L
lasts(L, R) :-
    lasts(L, [], R).
lasts([], A, A).
lasts([H], A, R) :-
    atomic(H),
    !,
    append(A, [H], NewA),
    lasts([], NewA, R).
lasts([H|T], A, R) :-
    atomic(H),
    T \= [],
    !,
    lasts(T, A, R).
lasts([H|T], A, R) :-
    lasts(H, A, A1),
    lasts(T, A1, R).
% ================================================================================================ EXERCISE 5

% The len_con_depth/2 predicate
% len_con_depth(L, R) replaces each constant depth sequence with its length
len_con_depth(L, R) :- 
    len_con_depth(L, 0, 0, R).

% len_con_depth(DeepList, isToSave, Length, Result)
len_con_depth([], 0,  _, []).
len_con_depth([], 1, Len, [Len]).
len_con_depth([H|T], _, Len, R) :-
    atomic(H),
    !,
    NewLen is Len + 1,
    len_con_depth(T, 1, NewLen, R).
len_con_depth([H|T], 0, _, [R1|R2]) :-
    !,
    len_con_depth(H, 0, 0, R1),
    len_con_depth(T, 0, 0, R2).
len_con_depth([H|T], 1, Len, [Len|[R1|R2]]) :-
    len_con_depth(H, 0, 0, R1),
    len_con_depth(T, 0, 0, R2).

% ================================================================================================ EXERCISE 6

% The depth_con_depth/2 predicate
% depth_con_depth(L, R) replaces each constant depth sequence with its depth
depth_con_depth(L, R) :-
    depth_con_depth(L, 0, 0, R).

% depth_con_depth(List, isToSave, Depth, Result)
depth_con_depth([], 0, _, []).
depth_con_depth([], 1, D, [D]).
depth_con_depth([H|T], _, D, R) :-
    atomic(H),
    !,
    depth_con_depth(T, 1, D, R).
depth_con_depth([H|T], 0, D, [R1|R2]) :-
    !,
    NewD is D + 1,
    depth_con_depth(H, 0, NewD, R1),
    depth_con_depth(T, 0, D, R2).
depth_con_depth([H|T], 1, D, [D|[R1|R2]]) :-
    NewD is D + 1,
    depth_con_depth(H, 0, NewD, R1),
    depth_con_depth(T, 0, D, R2).


% ================================================================================================ EXERCISE 7

member_deterministic(H, [H|_]) :-
    !.
member_deterministic(X, [H|_]) :-
    member_deterministic(X, H),
    !.
member_deterministic(X, [_|T]) :-
    member(X, T),
    !.

% ================================================================================================ EXERCISE 8

% The sort_depth/2 predicate
% sort_depth(L, R) sorts a deep list based on depth, if depths are equal, items are compared
sort_depth([H|T], R) :-
    sort_depth(T, R1),
    insert_ord(H, R1, R).
sort_depth([], []).

% The insert_ord/3 predicate
% insert_ord(X, L, R) inserts X in the ordered list L at the correct position
insert_ord(L, [H|T], [H|R]) :-
    is_before(H, L),
    !,
    insert_ord(L, T, R).
insert_ord(L, DL, [L|DL]).

% The is_before/2 predicate
% is_before(L1, L2) return true if L1 is before L2 in the sorted list
is_before(L1, L2) :-
    depth(L1, D1),
    depth(L2, D2),
    (
        (
            D1 < D2,
            !
        )
         ;
        (
            D1 = D2,
            !,
            is_before_equal_depths(L1, L2)
        )
    ).

% The is_before_equal_depths/2 predicate
% is_before_equal_depths(L1, L2) compares 2 lists with equal depths
is_before_equal_depths([], _).
is_before_equal_depths([H1|T1], [H2|T2]) :-
    atomic(H1),
    atomic(H2),
    number(H1),
    number(H2),
    H1 < H2,
    !,
    is_before_equal_depths(T1, T2).
is_before_equal_depths([H1|_], _) :-
    atomic(H1),
    !.
is_before_equal_depths([H1|T1], [H2|T2]) :-
    is_before_equal_depths(H1, H2),
    is_before_equal_depths(T1, T2).
