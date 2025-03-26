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
insert_ord([H|T], X, [H|R]) :-
    X > H,
    !,
    insert_ord(T, X, R).
insert_ord(L, X, [X|L]).

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

% The sort_lens/2 predicate
% sort_lens(LL, R) sorts in ascending order a list of lists based on the size of the elements
sort_lens(LL, [LMin|R]) :-
    min_lens(LL, LMin),
    !,
    delete_lens(LL, LMin, NewLL),
    sort_lens(NewLL, R).
sort_lens([], []).

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

% The list_length/2 predicate
list_length([_|T], NewR) :-
    list_length(T, R),
    NewR is R + 1.
list_length([], 0).

% The compareLexicographically/2 predicate
compareLexicographically([H0|T0], [H1|T1]) :-
    H0 =< H1,
    compareLexicographically(T0, T1).
compareLexicographically([], _).

% The delete_lens/3 predicate
delete_lens([LH|LT], L, LT) :-
    identical(LH, L),
    !.
delete_lens([LH|LT], L, [LH|LR]) :-
    delete_lens(LT, L, LR).

% The identical/2 predicate
identical([H|T0], [H|T1]) :-
    !,
    identical(T0, T1).
identical([], []).


