collect_parent(t(K, LT, RT), X, S, E) :-
    \+var(LT),
    \+var(RT),
    0 is K mod X,
    !,
    collect_parent(LT, X, S, [K|Inter]),
    collect_parent(RT, X, Inter, E).
collect_parent(t(_, LT, RT), X, S, E) :-
    \+var(LT),
    \+var(RT),
    !,
    collect_parent(LT, X, S, Inter),
    collect_parent(RT, X, Inter, E).
collect_parent(t(_, LT, _), X, S, E) :-
    \+var(LT),
    !,
    collect_parent(LT, X, S, E).
collect_parent(t(_, _, RT), X, S, E) :-
    \+var(RT),
    !,
    collect_parent(RT, X, S, E).
collect_parent(_, _, E, E).

extract_bigger(L, X, K, R) :-
    extract_bigger_helper(L, X, K, 1, R).

extract_bigger_helper([H|T], X, K, D, R) :-
    \+atomic(H),
    !,
    NewDepth is D + 1,
    extract_bigger_helper(H, X, K, NewDepth, R1),
    extract_bigger_helper(T, X, K, D, R2),
    append_il(R1, R2, R).
extract_bigger_helper([H|T], X, K, D, [H|R]) :-
    H > X,
    D =< K,
    !,
    extract_bigger_helper(T, X, K, D, R).
extract_bigger_helper([_|T], X, K, D, R) :-
    extract_bigger_helper(T, X, K, D, R).
extract_bigger_helper([], _, _, _, _).

append_il([H|_], L, L) :-
    var(H),
    !.
append_il([H|T], L, [H|R]) :-
    append_il(T, L, R).


collect([H|T], R) :-
    \+atomic(H),
    !,
    collect(H, R1),
    collect(T, R2),
    append_il(R1, R2, R).
collect([H|T], R) :-
    find_max(T, H, R).
collect([], _).

find_max([H|T], M, R) :-
    \+atomic(H),
    !,
    find_max(T, M, R1),
    collect(H, R2),
    append_il(R1, R2, R).
find_max([H|T], M, R) :-
    H < M,
    !,
    find_max(T, M, R).
find_max([H|T], _, R) :-
    find_max(T, H, R).
find_max([], M, [M|_]).

collect_tree(T, H, S, E) :-
    collect_tree(T, H, _, S, E).
collect_tree(t(K, LT, RT), H, CurrH, NewS, E) :-
    \+var(K),
    !,
    collect_tree(LT, H, CurrHL, S, Inter),
    collect_tree(RT, H, CurrHR, Inter, E),
    HL is CurrHL + 1,
    HR is CurrHR + 1,
    max(HL, HR, CurrH),
    (
        (
            H == CurrH, 
            NewS = [K|S],
            !
        );
        (
            NewS = S
        )
    ).
collect_tree(_, _, 0, E, E).

max(A, B, A) :-
    A > B,
    !.
max(_, B, B).

collect1([H|T], S, E) :-
    \+atomic(H),
    !,
    collect1(H, S, I),
    collect1(T, I, E).
collect1([H|T], S, E) :-
    find_min(T, H, S, E).
collect1([], E, E).

find_min([H|T], Min, S, E) :-
    \+atomic(H),
    !,
    collect1(H, S, I),
    find_min(T, Min, I, E).
find_min([H|T], Min, S, E) :-
    H > Min,
    !,
    find_min(T, Min, S, E).
find_min([H|T], _, S, E) :-
    find_min(T, H, S, E).
find_min([], Min, [Min|E], E).