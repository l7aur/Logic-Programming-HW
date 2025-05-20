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
depth_list_helper([H|T], D, R) :-
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

% The sum_k/3 predicate
% sum_k(L, D, R)

% The count_lists/2 predicate
% count_lists(L, R) counts the number of lists in a deep list

% The replace_all_deep/4 predicate
% replace_all_deep(X, Y, L, R)

% The len_con_depth/2 predicate
% len_con_depth(L, R) replaces each constatn depth sequence in a deep list with its length

% =========================================================================================== 4. TREES