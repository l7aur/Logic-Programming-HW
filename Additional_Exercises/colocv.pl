combinations(L1, L2, NewR) :-
    member(X, L1),
    !,
    delete(X, L1, NewL1),
    combinations(NewL1, L2, R),
    get_all(X, L2, CR),
    append(CR, R, NewR).

combinations([], _, []).

delete(X, [X|T], T) :-
    !.
delete(X, [H|T], [H|R]) :-
    delete(X, T, R).

get_all(X, [H|T], [[X, H]|R]) :-
    !,
    get_all(X, T, R).
get_all(_, [], []).

collect_tree(t(K, LT, RT), [K|R]) :-
    \+var(LT),
    \+var(RT),
    !,
    collect_tree(LT, R1),
    collect_tree(RT, R2),
    append(R1, R2, R).
collect_tree(t(_, _, RT), R) :-
    \+var(RT),
    !,
    collect_tree(RT, R).
collect_tree(t(_, LT, _), R) :-
    \+var(LT),
    !,
    collect_tree(LT, R).
collect_tree(_, []).


exclusive([H|T]) :-
    \+atomic(H),
    !,
    exclusive_helper(H),
    exclusive(T).
exclusive([]).

exclusive_helper([H|T]) :-
    atomic(H),
    !,
    exclusive_helper(T).
exclusive_helper([]) :-
    !.
exclusive_helper(L) :-
    exclusive(L).

collect_numbers([H|T], K, X, R) :-
    \+atomic(H),
    !,
    collect_numbers(H, K, X, R1),
    collect_numbers(T, K, X, R2),
    append_il(R1, R2, R).
collect_numbers([H|T], K, X, [H|R]) :-
    0 is H mod K,
    H < X,
    !,
    collect_numbers(T, K, X, R).
collect_numbers([_|T], K, X, R) :-
    collect_numbers(T, K, X, R).
collect_numbers([], _, _, _).

append_il([H|_], L, L) :-
    var(H),
    !.
append_il([H|T], L, [H|R]) :-
    append_il(T, L, R).

collect_even(T, S, E) :-
    collect_even_helper(T, 0, S, E).
collect_even_helper(t(K, LT, RT), D, S, E) :-
    \+var(K),
    1 is D mod 2,
    0 is K mod 2,
    !,
    NewD is D + 1,
    collect_even_helper(LT, NewD, S, [K|I]),
    collect_even_helper(RT, NewD, I, E).
collect_even_helper(t(K, LT, RT), D, S, E) :-
    \+var(K),
    !,
    NewD is D + 1,
    collect_even_helper(LT, NewD, S, I),
    collect_even_helper(RT, NewD, I, E).
collect_even_helper(_, _, E, E).

collect_bigger(T, X, R) :-
    collect_bigger_helper(T, 0, X, R).
collect_bigger_helper(t(K, LT, RT), D, X, R) :-
    \+var(K),
    0 is D mod 2,
    K > X,
    !,
    NewD is D + 1,
    collect_bigger_helper(LT, NewD, X, R1),
    collect_bigger_helper(RT, NewD, X, R2),
    append_il(R1, [K|R2], R).
collect_bigger_helper(t(K, LT, RT), D, X, R) :-
    \+var(K),
    !,
    NewD is D + 1,
    collect_bigger_helper(LT, NewD, X, R1),
    collect_bigger_helper(RT, NewD, X, R2),
    append_il(R1, R2, R).
collect_bigger_helper(_, _, _, _).

extract_bigger([H|T], K, X, S, E) :-
    \+atomic(H),
    !,
    extract_bigger(H, K, X, S, Inter),
    extract_bigger(T, K, X, Inter, E).
extract_bigger([H|T], K, X, [H|S], E) :-
    H > X,
    \+(0 is H mod K),
    !,
    extract_bigger(T, K, X, S, E).
extract_bigger([_|T], K, X, S, E) :-
    extract_bigger(T, K, X, S, E).
extract_bigger([], _, _, E, E).

collect_even_depth(L, K, S, E) :-
    collect_even_depth_helper(L, K, 0, S, E).
collect_even_depth_helper([H|T], K, D, S, E) :-
    \+atomic(H),
    !,
    NewD is D + 1,
    collect_even_depth_helper(H, K, NewD, S, Inter),
    collect_even_depth_helper(T, K, D, Inter, E).
collect_even_depth_helper([H|T], K, K, [H|S], E) :-
    0 is H mod 2,
    !,
    collect_even_depth_helper(T, K, K, S, E).
collect_even_depth_helper([_|T], K, D, S, E) :-
    collect_even_depth_helper(T, K, D, S, E).
collect_even_depth_helper([], _, _, E, E).

collect_one_child(t(_, LT, RT), X, Y, R) :-
    \+var(LT),
    \+var(RT),
    !,
    collect_one_child(LT, X, Y, R1),
    collect_one_child(RT, X, Y, R2),
    append_il(R1, R2, R).
collect_one_child(t(K, LT, _), X, Y, [K|R]) :-
    \+var(LT),
    X =< K,
    K =< Y,
    !,
    collect_one_child(LT, X, Y, R).
collect_one_child(t(_, LT, _), X, Y, R) :-
    \+var(LT),
    !,
    collect_one_child(LT, X, Y, R).
collect_one_child(t(K, _, RT), X, Y, [K|R]) :-
    \+var(RT),
    X =< K,
    K =< Y,
    !,
    collect_one_child(RT, X, Y, R).
collect_one_child(t(_, _, RT), X, Y, R) :-
    \+var(RT),
    !,
    collect_one_child(RT, X, Y, R).
collect_one_child(_, _, _, _).
