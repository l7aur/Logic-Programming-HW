% =========================================================================================== 1. ARITHMETIC OPERATIONS

% The gcd/3 predicate
% gcd(X, Y, Result)
gcd(X, Y, Result) :-
    Y \= 0,
    Rest is X mod Y,
    gcd(Y, Rest, Result).
gcd(X, 0, X).

% The lcm/3 predicate
% lcm(X, Y, Result)
lcm(X, Y, Result) :-
    gcd(X, Y, GCD),
    Result is X * Y / GCD.

% The divisor/2 predicate
% divisor(X, DivisorList)
divisor(X, DivisorList) :-
    divisor_helper(X, 1, DivisorList).
divisor(0, alot).
divisor_helper(X, Divisor, [Divisor|DivisorList]) :-
    Divisor < X,
    0 is X mod Divisor,
    !,
    NewDivisor is Divisor + 1,
    divisor_helper(X, NewDivisor, DivisorList).
divisor_helper(X, Divisor, DivisorList) :-
    Divisor < X,
    !,
    NewDivisor is Divisor + 1,
    divisor_helper(X, NewDivisor, DivisorList).
divisor_helper(X, X, [X]).

% The to_binary/2 predicate
% to_binary(N, B)
to_binary(N, B) :-
    to_binary_helper(N, [], B).
to_binary_helper(N, Acc, B) :-
    N \= 0,
    !,
    BinaryDigit is N mod 2,
    NewN is N div 2,
    to_binary_helper(NewN, [BinaryDigit|Acc], B).
to_binary_helper(0, Acc, Acc).

% The reverse/2 predicate
% reverse(N, ReversedN)
reverse(N, ReversedN) :-
    reverse_helper(N, 0, ReversedN).
reverse_helper(N, Acc, ReversedN) :-
    N \= 0,
    !,
    NewAcc is Acc * 10 + N mod 10,
    NewN is N div 10,
    reverse_helper(NewN, NewAcc, ReversedN).
reverse_helper(0, Acc, Acc).

% =========================================================================================== 2. OPERATIONS ON LISTS

% The sum/2 predicate
% sum(L, R)
sum(L, R) :-
    sum_helper(L, 0, R).
sum_helper([H|T], Acc, R) :-
    NewAcc is H + Acc,
    sum_helper(T, NewAcc, R).
sum_helper([], Acc, Acc).

% The numbers/2 predicate
% numbers(L, R) doubles the odd numbers and squares the even ones
numbers([H|T], [N|R]) :-
    0 is H mod 2,
    !,
    N is H * H,
    numbers(T, R).
numbers([H|T], [N|R]) :-
    N is 2 * H,
    numbers(T, R).
numbers([], []).

% The separate_parity/3 predicate
% separate_parity(L, Separated, Rest) separates the even elements on odd positions
separate_parity(L, Separated, Rest) :-
    separate_parity_helper(L, 1, Separated, Rest).
separate_parity_helper([H|T], Index, [H|Separated], Rest) :-
    0 is H mod 2,
    1 is Index mod 2,
    !,
    NewIndex is Index + 1,
    separate_parity_helper(T, NewIndex, Separated, Rest).
separate_parity_helper([H|T], Index, Separated, [H|Rest]) :-
    NewIndex is Index + 1,
    separate_parity_helper(T, NewIndex, Separated, Rest).
separate_parity_helper([], _, [], []).

% The replace_all/4 predicate
% replace_all(X, Y, L, R) replaces all Xs in L with Ys
replace_all(X, Y, [X|T], [Y|R]) :-
    !,
    replace_all(X, Y, T, R).
replace_all(X, Y, [H|T], [H|R]) :-
    !, % because of no indexation
    replace_all(X, Y, T, R).
replace_all(_, _, [], []).

% The replace_all/5 predicate
% replace_all(X, ListStart, ListEnd, Y, R) replaces all Xs in L with the sequence Y X Y
replace_all(X, [H|T], ListEnd, Y, [Y, X, Y|R]) :-
    \+var(H),
    X == H,
    [H|T] \= ListEnd,
    !,
    replace_all(X, T, ListEnd, Y, R).
replace_all(X, [H|T], ListEnd, Y, [H|R]) :-
    \+var(H),
    [H|T] \= ListEnd,
    !,
    replace_all(X, T, ListEnd, Y, R).
replace_all(_, _, _, _, []).

% The delete_pos_even/3 predicate
% delete_pos_even(L, X, R) deletes all occurences of X that are on an even positions
delete_pos_even(L, X, R) :-
    delete_pos_even_helper(L, X, 1, R).
delete_pos_even_helper([X|T], X, Index, R) :-
    0 is Index mod 2,
    !,
    NewIndex is Index + 1,
    delete_pos_even_helper(T, X, NewIndex, R).
delete_pos_even_helper([H|T], X, Index, [H|R]) :-
    !, % for indexation
    NewIndex is Index + 1,
    delete_pos_even_helper(T, X, NewIndex, R).
delete_pos_even_helper([], _, _, []).

% The delete_kth/3 predicate
% delete_kth(L, K, R) deletes each kth element in L
delete_kth(L, K, R) :-
    delete_kth_helper(L, K, 1, R).
delete_kth_helper([_|T], K, Index, R) :-
    0 is Index mod K,
    !,
    NewIndex is Index + 1,
    delete_kth_helper(T, K, NewIndex, R).
delete_kth_helper([H|T], K, Index, [H|R]) :-
    NewIndex is Index + 1,
    delete_kth_helper(T, K, NewIndex, R).
delete_kth_helper([], _, _, []).

% The delete_min/2 predicate
% delete_min(L, R) deletes all occurences of the minimum in the list
delete_min([H|T], R) :-
    delete_min_helper([H|T], H, _, R).
delete_min([], []).
delete_min_helper([H|T], CurrMin, Min, NewR) :-
    H < CurrMin,
    !,
    delete_min_helper(T, H, Min, R),
    update_delete_result(H, Min, R, NewR).
delete_min_helper([H|T], CurrMin, Min, NewR) :-
    delete_min_helper(T, CurrMin, Min, R),
    update_delete_result(H, Min, R, NewR).
delete_min_helper([], CurrMin, CurrMin, []).
update_delete_result(X, X, R, R) :-
    !.
update_delete_result(E, _, R, [E|R]).

% The delete_max/2 predicate
% delete_max(L, R) deletes all occurences of the maximum in the list
delete_max([H|T], R) :-
    delete_max_helper([H|T], H, _, R).
delete_max([], []).
delete_max_helper([H|T], CurrMax, Max, NewR) :-
    H > CurrMax,
    !,
    delete_max_helper(T, H, Max, R),
    update_delete_result(H, Max, R, NewR).
delete_max_helper([H|T], CurrMax, Max, NewR) :-
    delete_max_helper(T, CurrMax, Max, R),
    update_delete_result(H, Max, R, NewR).
delete_max_helper([], CurrMax, CurrMax, []).

% The delete_duplicates_keep_first/2 predicate
% delete_duplicates_keep_first(L, R)
delete_duplicates_keep_first(L, R) :-
    delete_duplicates_keep_first_helper(L, [], R).
delete_duplicates_keep_first_helper([H|T], Acc, R) :-
    member(H, Acc),
    !,
    delete_duplicates_keep_first_helper(T, Acc, R).
delete_duplicates_keep_first_helper([H|T], Acc, [H|R]) :-
    delete_duplicates_keep_first_helper(T, [H|Acc], R).
delete_duplicates_keep_first_helper([], _, []).

% The delete_duplicates_keep_last/2 predicate
% delete_duplicates_keep_last(L, R)
delete_duplicates_keep_last([H|T], R) :-
    member(H, T),
    !,
    delete_duplicates_keep_last(T, R).
delete_duplicates_keep_last([H|T], [H|R]) :-
    delete_duplicates_keep_last(T, R).
delete_duplicates_keep_last([], []).

% The reverse_il/2 predicate
% reverse_il(L, R) reverses an incomplete list
reverse_il(L, R) :-
    reverse_il_helper(L, _, R).
reverse_il_helper([H|T], Acc, R) :-
    \+var(H),
    !,
    reverse_il_helper(T, [H|Acc], R).
reverse_il_helper(_, Acc, Acc).

% The reverse_k/2 predicate
% reverse_k(L, K, R) reverses the elements of a list after the kth position
reverse_k(L, K, R) :-
    reverse_k_helper(L, K, [], R).
reverse_k_helper([H|T], K, Acc, [H|R]) :-
    K > 0,
    !,
    NewK is K - 1,
    reverse_k_helper(T, NewK, Acc, R).
reverse_k_helper([H|T], 0, Acc, R) :-
    reverse_k_helper(T, 0, [H|Acc], R).
reverse_k_helper([], _, Acc, Acc).

% The rle_encode/2 predicate
% rle_encode(L, R)
rle_encode([H|T], R) :-
    rle_encode_helper(T, H, 1, R).
rle_encode([], []).
rle_encode_helper([Symbol|T], Symbol, Count, R) :-
    !,
    NewCount is Count + 1,
    rle_encode_helper(T, Symbol, NewCount, R).
rle_encode_helper([NewSymbol|T], Symbol, Count, [[Symbol, Count]|R]) :-
    rle_encode_helper(T, NewSymbol, 1, R).
rle_encode_helper([], Symbol, Count, [[Symbol, Count]]).

% The rle_encode1/2 predicate
% rle_encode1(L, R)
rle_encode1([H|T], R) :-
    rle_encode1_helper(T, H, 1, R).
rle_encode1([], []).
rle_encode1_helper([Symbol|T], Symbol, Count, R) :-
    !,
    NewCount is Count + 1,
    rle_encode1_helper(T, Symbol, NewCount, R).
rle_encode1_helper([NewSymbol|T], Symbol, 1, [Symbol|R]) :-
    !,
    rle_encode1_helper(T, NewSymbol, 1, R).
rle_encode1_helper([NewSymbol|T], Symbol, Count, [[Symbol, Count]|R]) :-
    rle_encode1_helper(T, NewSymbol, 1, R).
rle_encode1_helper([], Symbol, Count, [[Symbol, Count]]).

% The rle_decode/2 predicate
% rle_decode(L, R)
rle_decode([H|T], R) :-
    \+atomic(H),
    !,
    rle_decode(T, PR),
    rle_expand(H, PR, R).
rle_decode([], []).
rle_expand([Symbol, Count], L, [Symbol|R]) :-
    Count > 0,
    !,
    NewCount is Count - 1,
    rle_expand([Symbol, NewCount], L, R).
rle_expand(_, L, L).

% The rotate_k/3 predicate
% rotate_k(L, K, R)
rotate_k(L, K, R) :-
    list_length(L, Len),
    Save is Len - K,
    rotate_k_helper(L, Save, FirstPart, LastPart),
    append_il(LastPart, FirstPart, R).
rotate_k_helper([H|T], Save, [H|FirstPart], LastPart) :-
    Save > 0,
    !,
    NewSave is Save - 1,
    rotate_k_helper(T, NewSave, FirstPart, LastPart).
rotate_k_helper(L, 0, _, L).

append_il([H|_], L, L) :-
    var(H),
    !.
append_il([H|T], L, [H|R]) :-
    append_il(T, L, R).

list_length([H|T], NewLen) :-
    atomic(H),
    !,
    list_length(T, Len),
    NewLen is Len + 1.
list_length(_, 0).


% The sort_chars/2 predicate
% sort_chars(L, R)
sort_chars(L, R) :-
    sort_chars_helper(L, [], R).
sort_chars_helper([H|T], Acc, R) :-
    insert_chars_in_place(Acc, H, NewAcc),
    sort_chars_helper(T, NewAcc, R).
sort_chars_helper([], Acc, Acc).

insert_chars_in_place([], X, [X]).
insert_chars_in_place([H|T], X, [H|R]) :-
    char_code(H, HValue),
    char_code(X, XValue),
    HValue < XValue, 
    !,
    insert_chars_in_place(T, X, R).
insert_chars_in_place([H|T], X, [X, H|T]).

% The sort_len/2 predicate
% sort_len(L, R)
sort_len(L, R) :-
    sort_len_helper(L, [], R).
sort_len_helper([H|T], Acc, R) :-
    insert_list_in_place(Acc, H, NewAcc),
    sort_len_helper(T, NewAcc, R).
sort_len_helper([], Acc, Acc).

insert_list_in_place([], X, [X]).
insert_list_in_place([H|T], X, [H|R]) :-
    length(H, LengthH),
    length(X, LengthX),
    LengthH < LengthX,
    !,
    insert_list_in_place(T, X, R).
insert_list_in_place([H|T], X, [X, H|T]).

% The remove_dup_on_odd_pos/2
% remove_dup_on_odd_pos(L, R)
remove_dup_on_odd_pos([H|T], R) :-
    remove_dup_on_odd_pos_helper([H|T], 1, [], R).
remove_dup_on_odd_pos([], []).

remove_dup_on_odd_pos_helper([H|T], Index, Acc, R) :-
    1 is Index mod 2,
    (member(H, Acc); member(H, T)),
    !,
    NewIndex is Index + 1,
    remove_dup_on_odd_pos_helper(T, NewIndex, [H|Acc], R).
remove_dup_on_odd_pos_helper([H|T], Index, Acc, [H|R]) :-   
    NewIndex is Index + 1,
    remove_dup_on_odd_pos_helper(T, NewIndex, [H|Acc], R).
remove_dup_on_odd_pos_helper([], _, _, []).

% =========================================================================================== 3. DEEP LISTS

maximum(X, Y, X) :-
    X > Y,
    !.
maximum(_, Y, Y).

% The depth_list/2 predicate
% depth_list(L, R)
depth_list(L, R) :-
    depth_list_helper(L, 1, R).
depth_list_helper([H|T], CurrDepth, NewR) :-
    \+atomic(H),
    !,
    NewDepth is CurrDepth + 1,
    depth_list_helper(H, NewDepth, R1),
    depth_list_helper(T, CurrDepth, R2),
    maximum(R1, R2, NewR).
depth_list_helper([_|T], D, R) :-
    depth_list_helper(T, D, R).
depth_list_helper([], D, D).

% The flatten/2 predicate
% flatten(L, R)
flatten([H|T], R) :-
    \+atomic(H),
    !,
    flatten(H, R1),
    flatten(T, R2),
    append(R1, R2, R).
flatten([H|T], [H|R]) :-
    flatten(T, R).
flatten([], []).

% The flatten_only_depth/3 predicate
% flatten_only_depth(L, D, R) 
flatten_only_depth(L, D, R) :-
    flatten_only_depth_helper(L, 1, D, R).
flatten_only_depth_helper([H|T], CurrDepth, D, R) :-
    \+atomic(H),
    !,
    NewDepth is CurrDepth + 1,
    flatten_only_depth_helper(H, NewDepth, D, R1),
    flatten_only_depth_helper(T, CurrDepth, D, R2),
    append(R1, R2, R).
flatten_only_depth_helper([H|T], CurrDepth, CurrDepth, [H|R]) :-
    !,
    flatten_only_depth_helper(T, CurrDepth, CurrDepth, R).
flatten_only_depth_helper([_|T], CurrDepth, D, R) :-
    flatten_only_depth_helper(T, CurrDepth, D, R).
flatten_only_depth_helper([], _, _, []).

% The sum_k/3 predicate
% sum_k(L, D, R)
sum_k(L, D, R) :-
    sum_k_helper(L, 1, D, R).
sum_k_helper([H|T], CurrDepth, D, R) :-
    \+var(H),
    \+atomic(H),
    !,
    NewDepth is CurrDepth + 1,
    sum_k_helper(H, NewDepth, D, R1),
    sum_k_helper(T, CurrDepth, D, R2),
    R is R1 + R2.
sum_k_helper([H|T], CurrDepth, CurrDepth, NewR) :-
    \+var(H),
    !,
    sum_k_helper(T, CurrDepth, CurrDepth, R),
    NewR is R + H.
sum_k_helper([H|T], CurrDepth, D, R) :-
    \+var(H),
    !,
    sum_k_helper(T, CurrDepth, D, R).
sum_k_helper(_, _, _, 0).

% The count_lists/2 predicate
% count_lists(L, R) counts the number of lists in a deep list
count_lists(L, R) :-
    count_lists_helper(L, 1, R).
count_lists_helper([H|T], Acc, R) :-
    \+atomic(H),
    !,
    NewAcc is Acc + 1,
    count_lists_helper(H, NewAcc, Acc1),
    count_lists_helper(T, Acc1, R).
count_lists_helper([_|T], Acc, R) :-
    count_lists_helper(T, Acc, R).
count_lists_helper([], Acc, Acc).

% The replace_all_deep/4 predicate
% replace_all_deep(X, Y, L, R)
replace_all_deep(X, Y, [H|T], R) :-
    \+atomic(H),
    !,
    replace_all_deep(X, Y, H, R1),
    replace_all_deep(X, Y, T, R2),
    append([R1], R2, R).
replace_all_deep(X, Y, [X|T], [Y|R]) :-
    !,
    replace_all_deep(X, Y, T, R).
replace_all_deep(X, Y, [H|T], [H|R]) :-
    !, 
    replace_all_deep(X, Y, T, R).
replace_all_deep(_, _, [], []).


% The len_con_depth/2 predicate
% len_con_depth(L, R) replaces each constatn depth sequence in a deep list with its length
len_con_depth(L, R) :-
    len_con_depth_helper(L, 0, R).
len_con_depth_helper([H|T], 0, R) :-
    \+atomic(H),
    !,
    len_con_depth_helper(H, 0, R1),
    len_con_depth_helper(T, 0, R2),
    append([R1], R2, R).
len_con_depth_helper([H|T], CurrLen, [CurrLen|R]) :-
    \+atomic(H),
        !,
    len_con_depth_helper(H, 0, R1),
    len_con_depth_helper(T, 0, R2),
    append([R1], R2, R).

len_con_depth_helper([_|T], CurrLen, R) :-
    NewLen is CurrLen + 1,
    len_con_depth_helper(T, NewLen, R).
len_con_depth_helper([], 0, []) :- 
    !.
len_con_depth_helper([], Len, [Len]).

% =========================================================================================== 4. TREES

tree(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree1(nil).
tree2(t(5, nil, _)).
tree3(t(5, t(7, nil, nil), nil)).
tree4(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))). 
tree5(t(3, t(2, t(1, nil, nil), t(4, nil, nil)), t(5, nil, nil))).

% The depth_tree/2 predicate
% depth_tree(Tree, Depth) computes the depth of a complete or incomplete binary tree
depth_tree(t(Key, LeftTree, RightTree), R) :-
    \+var(Key),
    !,
    depth_tree(LeftTree, R1),
    depth_tree(RightTree, R2),
    NewR1 is R1 + 1,
    NewR2 is R2 + 1,
    maximum(NewR1, NewR2, R).
depth_tree(_, 0).

% The inorder/2 predicate
% inorder(Tree, Result)
inorder(t(Key, LeftTree, RightTree), R) :-
    \+var(Key),
    !,
    inorder(LeftTree, R1),
    inorder(RightTree, R2),
    append(R1, [Key|R2], R).
inorder(_, []).

% The collect_k/2 predicate
% collect_k(Tree, R) collects all leaves of a tree
collect_k(t(Key, nil, nil), [Key]) :-
    !.
collect_k(t(_, LeftTree, RightTree), R) :-
    collect_k(LeftTree, R1),
    collect_k(RightTree, R2),
    append(R1, R2, R).
collect_k(nil, []).

% The is_bst/1 predicate
% is_bst(Tree)
is_bst(t(Key, t(KeyLeft, X, Y), t(KeyRight, Z, W))) :-
    KeyLeft < Key,
    Key < KeyRight,
    !,
    is_bst(t(KeyLeft, X, Y)),
    is_bst(t(KeyRight, Z, W)).
is_bst(t(Key, t(KeyLeft, X, Y), nil)) :-
    KeyLeft < Key,
    !,
    is_bst(t(KeyLeft, X, Y)).
is_bst(t(Key, nil, t(KeyRight, Z, W))) :-
    Key < KeyRight,
    !,
    is_bst(t(KeyRight, Z, W)).
is_bst(t(_, nil, nil)).

% The collect_odd_from_1child/2 predicate
% collect_odd_from_1child(T, R) collects all nodes with odd key and 1 child
collect_odd_from_1child(t(_, t(LeftKey, X, Y), t(RightKey, Z, W)), R) :-
    \+var(LeftKey),
    \+var(RightKey),
    !,
    collect_odd_from_1child(t(LeftKey, X, Y), R1),
    collect_odd_from_1child(t(RightKey, Z, W), R2),
    append_il(R1, R2, R).
collect_odd_from_1child(t(Key, t(LeftKey, X, Y), _), [Key|R]) :-
    \+var(LeftKey),
    1 is Key mod 2,
    !,
    collect_odd_from_1child(t(LeftKey, X, Y), R).
collect_odd_from_1child(t(_, t(LeftKey, X, Y), _), R) :-
    \+var(LeftKey),
    !,
    collect_odd_from_1child(t(LeftKey, X, Y), R).
collect_odd_from_1child(t(Key, _, t(RightKey, Z, W)), [Key|R]) :-
    \+var(RightKey),
    1 is Key mod 2,
    !,
    collect_odd_from_1child(t(RightKey, Z, W), R).
collect_odd_from_1child(t(_, _, t(RightKey, Z, W)), R) :-
    \+var(RightKey),
    !,
    collect_odd_from_1child(t(RightKey, Z, W), R).
collect_odd_from_1child(t(_, _, _), _).

% -----------------------------------------------------------------------------------------
ternary_tree(t(2,t(8,_,_,_),t(3,_,_,t(4,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(1,_,_,t(9,_,_,_))))).

% The collect_between/5 predicate
% collect_between(Tree, IntervalStart, IntervalEnd, ListStart, ListEnd) collects the items between IntervalStart and IntervalEnd in a difference list
collect_between(t(CurrKey, LeftTree, CentralTree, RightTree), IntervalStart, IntervalEnd, [CurrKey|StartR], EndR) :-
    \+var(CurrKey),
    IntervalStart =< CurrKey,
    CurrKey =< IntervalEnd,
    !,
    collect_between(LeftTree, IntervalStart, IntervalEnd, StartR, End1),
    collect_between(CentralTree, IntervalStart, IntervalEnd, End1, End2),
    collect_between(RightTree, IntervalStart, IntervalEnd, End2, EndR).
collect_between(t(CurrKey, LeftTree, CentralTree, RightTree), IntervalStart, IntervalEnd, StartR, EndR) :-
    \+var(CurrKey),
    !,
    collect_between(LeftTree, IntervalStart, IntervalEnd, StartR, End1),
    collect_between(CentralTree, IntervalStart, IntervalEnd, End1, End2),
    collect_between(RightTree, IntervalStart, IntervalEnd, End2, EndR).
collect_between(_, _, _, End, End).

% -----------------------------------------------------------------------------------------
tree10(t(5,t(10,t(7,nil,nil),t(10,t(4,nil,nil),t(3,nil,t(2,nil,nil)))),t(16,nil,nil))).

% The collect_even_from_leaf/3 predicate
% collect_even_from_leaf(Tree, ResultStart, ResultEnd) collects all even leaves from a binary tree in a difference lists
collect_even_from_leaf(t(Key, nil, nil), [Key|End], End) :-
    0 is Key mod 2,
    !.
collect_even_from_leaf(t(_, nil, nil), End, End) :-
    !.
collect_even_from_leaf(t(_, LeftTree, RightTree), Start, End) :-
    collect_even_from_leaf(LeftTree, Start, End1),
    collect_even_from_leaf(RightTree, End1, End).
collect_even_from_leaf(nil, End, End).

% -----------------------------------------------------------------------------------------
tree11(
    t(  2,
        t(8,_,_,_),
        t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(1,_,_,t(9,_,_,_))))).

% The replace_min/2 predicate
% replace_min(T, R) replaces the min in the tree with its root
replace_min(t(Key, LeftTree, CentralTree, RightTree), R) :-
    \+var(Key),
    !,
    find_tree_min(t(Key, LeftTree, CentralTree, RightTree), Min),
    replace_min_helper(t(Key, LeftTree, CentralTree, RightTree), Key, Min, R).
replace_min(_, _).

replace_min_helper(t(Key, LeftTree, CentralTree, RightTree), RootKey, Min, t(RootKey, RLeftTree, RCentralTree, RRightTree)) :-
    \+var(Key),
    Min == Key,
    !,
    replace_min_helper(LeftTree, RootKey, Min, RLeftTree),
    replace_min_helper(CentralTree, RootKey, Min, RCentralTree),
    replace_min_helper(RightTree, RootKey, Min, RRightTree).
replace_min_helper(t(Key, LeftTree, CentralTree, RightTree), RootKey, Min, t(Key, RLeftTree, RCentralTree, RRightTree)) :-
    \+var(Key),
    !,
    replace_min_helper(LeftTree, RootKey, Min, RLeftTree),
    replace_min_helper(CentralTree, RootKey, Min, RCentralTree),
    replace_min_helper(RightTree, RootKey, Min, RRightTree).
replace_min_helper(_, _, _, _).

find_tree_min(t(Key, LeftTree, CentralTree, RightTree), Min) :-
    \+var(Key),
    !,
    find_tree_min_helper(t(Key, LeftTree, CentralTree, RightTree), Key, Min).
find_tree_min(_, 999999999999999999).

find_tree_min_helper(t(Key, LeftTree, CentralTree, RightTree), CurrMin, Min) :-
    \+var(Key),
    !,
    find_tree_min_helper(LeftTree, CurrMin, NewMin),
    find_tree_min_helper(CentralTree, NewMin, NewMin2),
    find_tree_min_helper(RightTree, NewMin2, NewMin3),
    minimum(Key, NewMin3, Min).
find_tree_min_helper(_, Min, Min).

minimum(X, Y, Y) :-
    X > Y,
    !.
minimum(X, _, X).

% -----------------------------------------------------------------------------------------
tree12(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))).

% The collect_all_odd_depth/2 predicate
% collect_all_odd_depth(T, R) collects all nodes at odd depth in a binary tree
collect_all_odd_depth(T, R) :-
    collect_all_odd_depth_helper(T, 0, R).
collect_all_odd_depth_helper(t(Key, LeftTree, RightTree), CurrDepth, [Key|R]) :-
    \+var(Key),
    1 is CurrDepth mod 2,
    !,
    NewDepth is CurrDepth + 1,
    collect_all_odd_depth_helper(LeftTree, NewDepth, R1),
    collect_all_odd_depth_helper(RightTree, NewDepth, R2),
    append(R1, R2, R).
collect_all_odd_depth_helper(t(Key, LeftTree, RightTree), CurrDepth, R) :-
    \+var(Key),
    !,
    NewDepth is CurrDepth + 1,
    collect_all_odd_depth_helper(LeftTree, NewDepth, R1),
    collect_all_odd_depth_helper(RightTree, NewDepth, R2),
    append(R1, R2, R).
collect_all_odd_depth_helper(_, _, []).

% -----------------------------------------------------------------------------------------
tree13(t(2,t(8,_,_,_),t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(5,_,_,_),t(1,_,_,t(9,_,_,_))))).

% [t(5, t(7, _A, _B, _C), t(5, _D, _E, _F), t(1, _G, _H, t(9, _I, _J, _K))), 
% t(7, _A, _B, _C), 
% t(5, _D, _E, _F), 
% t(1, _G, _H, t(9, _I, _J, _K)), 
% t(9, _I, _J, _K)]

% The median/2 predicate
% median(T, R) collects all subtrees having the median value the same as the median of the whole tree
median(t(Key, LeftTree, CentralTree, RightTree), R) :-
    \+var(Key),
    !,
    median_helper(t(Key, LeftTree, CentralTree, RightTree), Subtrees, NodeList),
    median_sort_2level_list(NodeList, SortedList),
    median_get(SortedList, Medians),
    median_collect_trees(Medians, Subtrees, R).
median(_, []).

median_helper(t(Key, LeftTree, CentralTree, RightTree), [t(Key, LeftTree, CentralTree, RightTree)|Subtrees], NodeList) :-
    \+var(Key),
    !,
    median_helper(LeftTree, Subtrees1, NodeList1),
    median_helper(CentralTree, Subtrees2, NodeList2),
    median_helper(RightTree, Subtrees3, NodeList3),
    % create the list of nodes corresponding to the current tree
    append(NodeList2, NodeList3, IntermNodeList),
    append(NodeList1, IntermNodeList, NodeList0),
    take_top(NodeList1, NodeList2, NodeList3, SimplifiedNodeList0),
    flatten(SimplifiedNodeList0, FlatList),
    append([[Key|FlatList]], NodeList0, NodeList),
    % save the processed subtrees
    append(Subtrees2, Subtrees3, IntermSubtrees),
    append(Subtrees1, IntermSubtrees, Subtrees).
median_helper(_, [], []).

take_top([H|_], [H1|_], [H2|_], [H, H1, H2]).
take_top([], [H|_], [H1|_], [H, H1]).
take_top([H|_], [], [H1|_], [H, H1]).
take_top([H|_], [H1|_], [], [H, H1]).
take_top([H|_], [], [], [H]).
take_top([], [H|_], [], [H]).
take_top([], [], [H|_], [H]).
take_top([], [], [], []).

median_collect_trees([H|T], [_|Trees], R) :-
    !,
    median_collect_trees_helper(T, H, Trees, R).
median_collect_trees(_, _, []).

median_collect_trees_helper([Median|T], Median, [Tree|Rest], [Tree|R]) :-
    !,
    median_collect_trees_helper(T, Median, Rest, R).
median_collect_trees_helper([_|T], Median, [_|Rest], R) :-
    median_collect_trees_helper(T, Median, Rest, R).
median_collect_trees_helper([], _, _, []).

median_get([H|T], R) :-
    \+atomic(H),
    !,
    median_get(H, R1),
    median_get(T, R2),
    append([R1], R2, R).
median_get([], []) :- 
    !.
median_get(L, R) :-
    length(L, Len),
    NewLen is Len div 2,
    median_get_helper(L, NewLen, R).

median_get_helper([_|T], Len, R) :-
    Len > 0,
    !,
    NewLen is Len - 1,
    median_get_helper(T, NewLen, R).
median_get_helper([H|_], 0, H).

median_sort_2level_list([H|T], R) :-
    \+atomic(H),
    !,
    median_sort_2level_list(H, R1),
    median_sort_2level_list(T, R2),
    append([R1], R2, R).
median_sort_2level_list(L, R) :-
    sort_list(L, R).

sort_list(L, R) :-
    sort_list_helper(L, [], R).
sort_list_helper([H|T], A, R) :-
    insert_in_place(H, A, NewA),
    sort_list_helper(T, NewA, R).
sort_list_helper([], A, A).

insert_in_place(X, [H|T], [H|R]) :-
    H < X,
    !,
    insert_in_place(X, T, R).
insert_in_place(X, [H|T], [X, H|T]) :-
    !.
insert_in_place(X, [], [X]).

% -----------------------------------------------------------------------------------------
tree14(t(2,t(4,t(5,_,_),t(7,_,_)),t(3,t(0,t(4,_,_),_),t(8,_,t(5,_,_))))). 

% The height_each/2 predicate
% height_each(T, R) replaces the key of each node with its height
height_each(T, R) :-
    height_each_helper(T, _, R).
height_each_helper(t(Key, LeftTree, RightTree), Height, t(Height, RLeftTree, RRightTree)) :-
    \+var(Key),
    !,
    height_each_helper(LeftTree, Height1, RLeftTree),
    height_each_helper(RightTree, Height2, RRightTree),
    CurrHeight1 is Height1 + 1,
    CurrHeight2 is Height2 + 1,
    maximum(CurrHeight1, CurrHeight2, Height).
height_each_helper(_, -1, _).

% The sum_subtree/2 predicate
% sum_subtree(T, K, R) replaces the any subtree whose key is K with the sum of the keys of the subtree