% The intersect/3 predicate
% intersect(A, B, R) computes the intersection between A and B
% with CUT
intersect([HA|TA], B, [HA|R]) :-
    member(HA, B),
    !,
    intersect(TA, B, R).
intersect([_|TA], B, R) :-
    intersect(TA, B, R).
intersect([], _, []).

% without CUT
intersect1([HA|TA], B, [HA|R]) :-
    member(HA, B),
    intersect1(TA, B, R).
intersect1([HA|TA], B, R) :-
    not(member(HA, B)),
    intersect1(TA, B, R).
intersect1([], _, []).

% The diff/3 predicate
% diff(A, B, R) computes the difference between the sets A and B
% with CUT
diff([HA|TA], B, R) :-
    member(HA, B),
    !,
    diff(TA, B, R).
diff([HA|TA], B, [HA|R]) :-
    diff(TA, B, R).
diff([], _, []).

% without CUT
diff1([HA|TA], B, R) :-
    member(HA, B),
    diff1(TA, B, R).
diff1([HA|TA], B, [HA|R]) :-
    not(member(HA, B)),
    diff1(TA, B, R).
diff1([], _, []).

% The del_min/2 predicate
% del_min(L, R) deletes all occurences of the minimum in the list L 
del_min(L, R) :-
    find_min(L, Min),
    del_minHelper(L, Min, R).

del_minHelper([H|T], Min, R) :-
    number(H),
    Min == H,
    !,
    del_minHelper(T, Min, R).
del_minHelper([H|T], Min, [H|R]) :-
    del_minHelper(T, Min, R).
del_minHelper([], _, []).

% The find_min/2 predicate
% find_min(L, R) returns the minimum element inside L in R
find_min([H|T], Min) :-
    not(number(H)),
    !,
    find_min(T, Min).
find_min([H|T], Min) :-
    find_min(T, Min, H).

% The find_min/3 predicate
% find_min(L, R, Ini) stores the first number in Min and continues minimum calculation
find_min([H|T], Min, Ini) :-
    not(number(H)),
    !,
    find_min(T, Min, Ini).
find_min([H|T], Min, Ini) :-
    find_min(T, Min, Ini),
    Min < H,
    !.
find_min([H|T], H, Ini) :-
    find_min(T, _, Ini).
find_min([], Ini, Ini).

% The del_max/2 predicate
% del_max(L, R) deletes all occurences of the maximum in the list L 
del_max(L, R) :-
    find_max(L, Max),
    del_maxHelper(L, Max, R).

del_maxHelper([H|T], Max, R) :-
    number(H),
    Max == H,
    !,
    del_maxHelper(T, Max, R).
del_maxHelper([H|T], Max, [H|R]) :-
    del_maxHelper(T, Max, R).
del_maxHelper([], _, []).

% The find_max/2 predicate
% find_max(L, R) returns the maximum element inside L in R
find_max([H|T], Max) :-
    not(number(H)),
    !,
    find_max(T, Max).
find_max([H|T], Max) :-
    find_max(T, Max, H).

% The find_max/3 predicate
% find_max(L, R, Ini) stores the first number in Max and continues maximum calculation
find_max([H|T], Max, Ini) :-
    not(number(H)),
    !,
    find_max(T, Max, Ini).
find_max([H|T], Max, Ini) :-
    find_max(T, Max, Ini),
    Max > H,
    !.
find_max([H|T], H, Ini) :-
    find_max(T, _, Ini).
find_max([], Ini, Ini).

% The reverse_k/3 predicate
% reverse_k(L, K, R) reverses the elements starting at position K in L
reverse_k([H|T], 0, R) :-
    reverse_k(T, 0, NewR),
    append(NewR, [H], R).
reverse_k([H|T], K, [H|R]) :-
    K > 0,
    NewK is K - 1,
    reverse_k(T, NewK, R).
reverse_k([], _, []).

% The rle_encode/2 predicate
% rle_encode(L, R) performs run length encoding on list L and returns the result in R
% [1,1,1,2,3,4] => [[1, 3], [2, 1], [3, 1], [4, 1]] 
rle_encode(L, R) :-
    rle_encode(L, R, 1).
rle_encode([H|[H|T]], R, I) :-
    !,
    NewI is I + 1,
    rle_encode([H|T], R, NewI).
rle_encode([H|[X|T]], [[H, I]|R], I) :-
    !,
    rle_encode([X|T], R, 1).
rle_encode([H|T], [[H, I]|R], I) :-
    rle_encode(T, R, 1).
rle_encode([], [], _).

% The rle_encode1/2 predicate
% rle_encode1(L, R) performs run length encoding on list L and returns the result in R
% [1,1,1,2,3,4] => [[1, 3], 2, 3, 4] 
rle_encode1(L, R) :-
    rle_encode1(L, R, 1).
rle_encode1([H|[H|T]], R, I) :-
    !,
    NewI is I + 1,
    rle_encode1([H|T], R, NewI).
rle_encode1([H|[X|T]], [H|R], 1) :-
    !,
    rle_encode1([X|T], R, 1).
rle_encode1([H|[X|T]], [[H, I]|R], I) :-
    !,
    rle_encode1([X|T], R, 1).
rle_encode1([H|T], [H|R], 1) :-
    !,
    rle_encode1(T, R, 1).
rle_encode1([H|T], [[H, I]|R], I) :-
    rle_encode1(T, R, 1).
rle_encode1([], [], _).

% The rle_decode/2 predicate
% rle_decode(L, R) decodes a run-length encoded list and returns the result in R.
rle_decode(L, R) :-
    rle_decode(L, R, -1).
% The rle_decode/3 predicate - helper
% rle_decode(L, R, I) uses I as a state variable: -1 -> uninitialized counter, thus set it
%                                                  0 -> counter reached 0, need a new initialization
%                                                > 0 -> insert copies of the element in the list and decrement
rle_decode([[E|I]|T], R, -1) :-
    rle_decode([[E|I]|T], R, I),
    !.
rle_decode([[_]|T], R, 0) :-
    rle_decode(T, R, -1).
rle_decode([[E|_]|T], [E|R], I) :-
    I > 0,
    NewI is I - 1,
    rle_decode([[E|_]|T], R, NewI).
rle_decode([], [], _).
