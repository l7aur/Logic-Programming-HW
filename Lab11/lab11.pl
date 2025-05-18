% ================================================================================================  PREDEFINED IN THE LAB

:- discontiguous edge/2.
edge(1,2).
edge(1,5).
edge(2,3).
edge(2,5).
edge(3,4).
edge(4,5).
edge(4,6).

is_edge(X, Y) :-
    edge(X, Y);
    edge(Y, X).

:- dynamic visited_node/1.
% dfs(source, path)
dfs(X, _) :-
    df_search(X).
dfs(_, L) :-
    !,
    collect_reverse([], L).

% traversal predicate
df_search(X) :-
    asserta(visited_node(X)),
    is_edge(X, Y),
    not(visited_node(Y)),
    df_search(Y).

% collecting predicate - collects in reverse order
collect_reverse(L, P) :-
    retract(visited_node(X)),
    !,
    collect_reverse([X|L], P).
collect_reverse(L, L).

:- dynamic visited_node/1. 
:- dynamic queue/1.
 
% bfs(Source, Path) 
bfs(X, _):-
    assertz(visited_node(X)),
    assertz(queue(X)),
    bf_search.  
bfs(_,R):- 
    !, 
    collect_reverse([],R). 
 
bf_search:- 
    retract(queue(X)),
    expand(X), 
    !,
    bf_search.
 
expand(X):-  
    is_edge(X,Y),
    not(visited_node(Y)),
    asserta(visited_node(Y)),
    assertz(queue(Y)),
    fail.
expand(_). 

pos_vec(start, 0, 2, [a, d]).
pos_vec(a, 2, 0, [start, b]).
pos_vec(b, 5, 0, [a, c, end]).
pos_vec(c, 10, 0, [b, end]).
pos_vec(d, 3, 4, [start, e]).
pos_vec(e, 7, 4, [d]).
pos_vec(end, 7, 2, [b, c]).

is_target(end).

best([], []) :- 
    !.
best([[Target|Rest]|_], [Target|Rest]) :-
    is_target(Target),
    !.
best([[H|T]|Rest], Best) :-
    pos_vec(H, _, _, Neighbour),
    expand(Neighbour, [H|T], Rest, Exp),
    quick_sort(Exp, SortExp, []),
    best(SortExp, Best).

expand([], _, Exp, Exp) :-
    !.
expand([H|T], Path, Rest, Exp) :-
    \+(member(H, Path)),
    !,
    expand(T, Path, [[H|Path]|Rest], Exp).
expand([_|T], Path, Rest, Exp) :-
    expand(T, Path, Rest, Exp).

quick_sort([H|T], S, E) :-
    partition(H, T, A, B),
    quick_sort(A, S, [H|Y]),
    quick_sort(B, Y, E).
quick_sort([], S, S).

partition(H, [A|X], [A|Y], Z) :-
    order(A, H),
    !,
    partition(H, X, Y, Z).
partition(H, [A|X], Y, [A|Z]) :-
    partition(H, X, Y, Z).
partition(_, [], [], []).

dist(Node1, Node2, Dist) :-
    pos_vec(Node1, X1, Y1, _),
    pos_vec(Node2, X2, Y2, _),
    Dist is (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2).

order([Node1|_], [Node2|_]) :-
    is_target(Target),
    dist(Node1, Target, Dist1),
    dist(Node2, Target, Dist2),
    Dist1 < Dist2.

% ================================================================================================  EXERCISE 1

edge(a, b). 
edge(a, c). 
edge(b, d). 
edge(d, e). 
edge(c, f). 
edge(e, g). 
edge(f, h).

depth_max(2).

dls(X, _) :- 
    depth_max(Depth),
    not(var(Depth)),
    df_search_dls(X, Depth).
dls(_, L) :-
    !,
    collect_reverse_dls([], L).

df_search_dls(X, Depth) :-
    Depth >= 0,
    NewDepth is Depth - 1,
    asserta(visited_node(X)),
    is_edge(X, Y),
    not(visited_node(Y)),
    df_search_dls(Y, NewDepth).

collect_reverse_dls(L, P) :-
    retract(visited_node(X)),
    !,
    collect_reverse_dls([X|L], P).
collect_reverse_dls(L, L).

% ================================================================================================  EXERCISE 2
% neighbour(a, [b, c, x]).
% neighbour(b, [a, d]).
% neighbour(c, [a, e, z, w]).
% neighbour(d, [b]).
% neighbour(e, [c, y]).
% neighbour(x, [a]).
% neighbour(y, [e]).
% neighbour(z, [c]).
% neighbour(w, [c]).

% neighbour(a, [b, c]).
% neighbour(b, [a, c]).
% neighbour(c, [a, b]).

neighbour(a, [b,c]). 
neighbour(b, [a,d]). 
neighbour(c, [a,e]). 
neighbour(d, [b]). 
neighbour(e, [c]).

bfs1(X, R) :- 
    bfs1([X], [], P), reverse(P, R).

bfs1([], V, V). 
bfs1([X|Q], V, R):-  
    \+member(X, V), 
    neighbour(X, Ns), 
    remove_visited(Ns, V, RemNs), 
    append(Q, RemNs, NewQ), 
    bfs1(NewQ, [X|V], R). 
 
remove_visited([], _, []). 
remove_visited([H|T], V, [H|R]) :- 
    \+member(H, V), 
    !, 
    remove_visited(T, V, R). 
remove_visited([_|T], V, R) :- 
    remove_visited(T, V, R). 

% ================================================================================================  EXERCISE 2.1

bfs2(X, R) :-
    bfs2([X], [], R).
bfs2([], _, []).
bfs2([X|Q], V, [X|R]) :- % move pattern here
    \+member(X, V),
    neighbour(X, Ns),
    remove_visited(Ns, V, RemNs),
    append(Q, RemNs, NewQ),
    bfs2(NewQ, [X|V], R).

% difference lists
bfs22(X, R) :-
    bfs22([X|EndQ], EndQ, [], Var, Var, R).
bfs22(Q, Q, _, Start, [], Start) :- 
    var(Q),
    !.
bfs22([X|Q], EndQ, V, Start, End, R) :-
    \+member(X, V),
    neighbour(X, Ns),
    remove_visited(Ns, V, RemNs),
    convertCL2DL(RemNs, StartNs, EndNs),
    add_dl(X, Start, End, NewStart, NewEnd),
    append_dl(Q, EndQ, StartNs, EndNs, NewQS, NewQE),
    bfs22(NewQS, NewQE, [X|V], NewStart, NewEnd, R).
    
add_dl(X, LS, [X|RE], LS, RE).

append_dl(LS1, Link, Link, LE2, LS1, LE2).

convertCL2DL([H|T], [H|S], E) :-
    convertCL2DL(T, S, E).
convertCL2DL([], E, E).

% ================================================================================================  EXERCISE 2.2
edge22(a, b).
edge22(a, c).
edge22(b, a).
edge22(b, d).
edge22(c, a).
edge22(c, e).
edge22(d, b).
edge22(e, c).
% edge22(a, x).
% edge22(c, z).
% edge22(c, w).
% edge22(e, y).

bfs3(X, R) :-
    bfs3([X], [], R).
bfs3([], _, []).
bfs3([X|Q], V, [X|R]) :-
    \+member(X, V),
    findall(Y, edge22(X, Y), NewQ),
    remove_visited(NewQ, [X|V], NQ),
    append(Q, NQ, NewNewQ),
    bfs3(NewNewQ, [X|V], R).