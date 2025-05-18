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

% =========================================================================================== 1. OPERATIONS ON LISTS

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
% replace_all(X, ReplacementStart, ReplacementEnd, L, R) replaces all Xs in L with the difference list Replacement
    