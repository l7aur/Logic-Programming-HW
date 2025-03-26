% The max/2 predicate
% max(L, R) computes the maximum in the list L
max([H|T], M) :-
    max(T, M),
    M > H,
    !.
max([H|_], H).

% The delete_first_occurence/3 predicate
% delete_first_occurence(L, X, R) deletes the first occurence of X in L
delete_first_occurence([H|T], H, T) :-
    !.
delete_first_occurence([H|T], X, [H|R]) :-
    delete_first_occurence(T, X, R).
delete_first_occurence([], _, []).

% The sel_sort_max/2 predicate
% sel_sort_max(L, R) sorts the list in descending order
sel_sort_max(L, [M|R]) :-
    max(L, M),
    !,
    delete_first_occurence(L, M, NewL),
    sel_sort_max(NewL, R).
sel_sort_max([], []).

% The ins_sort_fwd/2 predicate
% ins_sort_fwd(L, R) sorts a list in ascending order using insertion sort + forward recursion
ins_sort_fwd(L, R) :-
    ins_sort_fwd(L, [], R).
ins_sort_fwd([H|T], Acc, R) :-
    insert_ord(Acc, H, NewAcc),
    ins_sort_fwd(T, NewAcc, R).
ins_sort_fwd([], Acc, Acc).

% The insert_ord/3 predicate
% insert_ord(L, X, R) inserts X in the ordered list L at the required position
insert_ord(L, X, R) :- 
    insert_ord(L, X, [], R).
insert_ord([H|T], X, Acc, R) :-
    X > H,
    !,
    append(Acc, [H], NewAcc),
    insert_ord(T, X, NewAcc, R).
insert_ord(L, X, Acc, R) :-
    append(Acc, [X|L], R).

% The sort_chars/2 predicate
% sort_chars(L, R) sorts a list of chars in ascending order based on quicksort
sort_chars([H|T], R) :-
    partition(H, T, Smaller, Larger),
    sort_chars(Smaller, SortedSmaller),
    sort_chars(Larger, SortedLarger),
    append(SortedSmaller, [H|SortedLarger], R).
sort_chars([], []).

% The partition/4 predicate
% partition(P, L, Smaller, Larger) decomposes L into 2 lists based on the order relation of the elements with P
partition(P, [H|T], Smaller, [H|Larger]) :-
    char_code(P, PVal),
    char_code(H, HVal),
    HVal > PVal,
    !,
    partition(P, T, Smaller, Larger).
partition(P, [H|T], [H|Smaller], Larger) :- 
    partition(P, T, Smaller, Larger).
partition(_, [], [], []).

% The list_length/2 predicate
% list_length(L, R) computes the length of the list L
list_length([_|T], NewR) :-
    list_length(T, R),
    NewR is R + 1.
list_length([], 0).

% The compareLexicographically/2 predicate
% compareLexicographically(L1, L2) returns true if L1 is in front of L2 in lexicographical order 
compareLexicographically([H0|T0], [H1|T1]) :-
    H0 =< H1,
    compareLexicographically(T0, T1).
compareLexicographically([], _).

% The delete_lens/3 predicate
% delete_lens(L, X, R) deletes one occurence of X in L 
delete_lens([LH|LT], L, LT) :-
    identical(LH, L),
    !.
delete_lens([LH|LT], L, [LH|LR]) :-
    delete_lens(LT, L, LR).

% The identical/2 predicate
% identical(L1, L2) returns true if L1 is the same as L2
identical([H|T0], [H|T1]) :-
    !,
    identical(T0, T1).
identical([], []).

% The min_lens/3 predicate
% min_lens(LL, LR) returns the smallest list in length if unique, otherwise returns the first one in lexicographic order
min_lens([LH|LT], LR) :-
    list_length(LH, Len),
    min_lens(LT, LH, Len, LR).
min_lens([LH|LT], LAcc, LenAcc, LR) :-
    list_length(LH, LenAcc),
    compareLexicographically(LH, LAcc),
    !,
    min_lens(LT, LH, LenAcc, LR).
min_lens([LH|LT], _, LenAcc, LR) :-
    list_length(LH, LenLH),
    LenLH < LenAcc,
    !,
    min_lens(LT, LH, LenLH, LR).
min_lens([_|LT], LAcc, LenAcc, LR) :-
    min_lens(LT, LAcc, LenAcc, LR).
min_lens([], LAcc, _, LAcc).

% The sort_lens2/2 predicate
% sort_lens2(LL, R) sorts in ascending order based on selection sort a list of lists based on the size of the elements
sort_lens2(LL, [LMin|R]) :-
    min_lens(LL, LMin),
    !,
    delete_lens(LL, LMin, NewLL),
    sort_lens2(NewLL, R).
sort_lens2([], []).

% The bubble_sort_fixed/3 predicate
% bubble_sort_fixed(L, K, R) does K one-item-bubble-up iterations
bubble_sort_fixed(L, K, R) :- 
    one_pass(L, R1, F), 
    nonvar(F), 
    K > 0,
    !,
    NewK is K - 1,
    bubble_sort_fixed(R1, NewK, R).
bubble_sort_fixed(L, _, L).

% The one_pass predicate
% one_pass(L, R, F) does a one-item-bubble-up
one_pass([H1, H2|T], [H2|R], F) :-
    H1 > H2,
    !,
    F = 1,
    one_pass([H1|T], R, F).
one_pass([H1|T], [H1|R], F) :-
    one_pass(T, R, F).
one_pass([], [], _).

% The perm1/2 predicate
% perm1(L, R) generates the permutations of L
perm1(L, [RA|RT]) :-
    delete1(RA, L, NewR),
    perm1(NewR, RT).
perm1([], []).

% The delete1/3 predicate
% delete(X, L, R) deletes the first occurence of X in L
delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]) :-
    delete1(X, T, R).


    