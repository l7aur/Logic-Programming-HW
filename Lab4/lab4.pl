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

% The del_min/3 predicate
% del_min(L, R) deletes all occurences of the minimum in the list L 


% The find_min/2 predicate
% find_min(L, R) returns the minimum element inside L in R
find_min([H|T], R) :-
    not(number(H)),
    find_min(T, R).
find_min([H|T], H) :-
    number(H),
    find_min(T, Min),
    Min > H.
find_min([H|T], R) :-
    number(H),
    find_min(T, R),
    R < H.
find_min([], 9999999999).