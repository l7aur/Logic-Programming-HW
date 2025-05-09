% ========================================================================= PREDEFINED IN THE LAB

:- dynamic neighbor/2.
neighbor(a, [b, d]).
neighbor(b, [a, c, d]).
neighbor(c, [b, d]).

neighb_to_edge :-
    retract(neighbor(Node, List)),
    !,
    process(Node, List),
    neighb_to_edge.
neighb_to_edge.

neighb_to_edge_v2 :-
    neighbor(Node, List),
    process(Node, List),
    fail.
neighb_to_edge_v2.

:- dynamic seen/1.
neighb_to_edge_v3 :-
    neighbor(Node, List),
    not(seen(Node)),
    !,
    assert(seen(Node)),
    process(Node, List),
    neighb_to_edge_v3.
neighb_to_edge_v3.

process(Node, [H|T]) :-
    assertz(gen_edge(Node, H)),
    process(Node, T).
process(Node, []) :-
    assertz(gen_node(Node)).

edge(a, b).
edge(a, d).
edge(b, a).
edge(b, c).
edge(b, d).
edge(c, b).
edge(c, d).
edge(d, c).
edge(d, a).

path(X, Y, Path) :-
    path(X, Y, [X], Path).
path(Y, Y, PartialPath, PartialPath).
path(X, Y, PartialPath, R) :-
    edge(X, Z),
    not(member(Z, PartialPath)),
    path(Z, Y, [Z|PartialPath], R).

restricted_path(X, Y, LR, P) :-
    path(X, Y, P),
    reverse(P, PR),
    check_restrictions(LR, PR).

check_restrictions([], _) :-
    !.
check_restrictions([H|TR], [H|TP]) :-
    !,
    check_restrictions(TR, TP).
check_restrictions(LR, [_|TP]) :-
    check_restrictions(LR, TP).

:-dynamic sol_part/2.

optimal_path(X, Y, Path) :-
    asserta(sol_part([], 100)),
    path1(X, Y, [X], Path, 1).
optimal_path(_, _, Path) :-
    retract(sol_part(Path, _)).

path1(Y, Y, Path, LPath) :-
    retract(sol_part(_, _)),
    !,
    asserta(sol_part(Path, LPath)),
    fail.
path1(X, Y, PPath, FPath, LPath) :-
    edge(X, Z),
    not(member(X, Z)),
    LPath1 is LPath + 1,
    sol_part(_, Lopt),
    LPath < Lopt,
    path1(Z, Y, [Z|PPath], FPath, LPath1).

% ========================================================================= EXERCISE 1
% [NOTE] At first, I considered the graph to be oriented and printed the result accordingly
% [NOTE] That's why the output is different, however the result is still ok

% graph1
edge_ex1(a, b). 
edge_ex1(b, c). 
edge_ex1(a, c). 
edge_ex1(c, d). 
edge_ex1(b, d). 
edge_ex1(d, e). 
edge_ex1(e, a).

% graph2
% edge_ex1(0, 2).
% edge_ex1(0, 4).
% edge_ex1(0, 5).
% edge_ex1(1, 4).
% edge_ex1(1, 5).
% edge_ex1(2, 3).
% edge_ex1(2, 4).
% edge_ex1(4, 5).

is_edge_ex1(X, Y) :-
    edge_ex1(X, Y);
    edge_ex1(Y, X).

% The hamilton/3 predicate
% hamilton(NumberOfNodes, Source, Path) finds a hamiltonian cycle of length NumberOfNodes or returns false
hamilton(NN, X, Path) :-
    NN1 is NN - 1,
    hamilton_path(NN1, X, X, [X], Path),
    !. % just the first found solution

hamilton_path(0, Source, Target, _, [Source, Target]) :-
    is_edge_ex1(Source, Target),
    !.
hamilton_path(N, Source, Target, Visited, [Source|Path]) :-
    NewN is N - 1,
    % edge_ex1(Source, Neighbour), % for directed graph
    is_edge_ex1(Source, Neighbour), % for undirected graph
    not(member(Neighbour, Visited)),
    hamilton_path(NewN, Neighbour, Target, [Neighbour|Visited], Path).

% ========================================================================= EXERCISE 2
% graph1
% edge_ex2(a, b). 
% edge_ex2(b, e). 
% edge_ex2(c, a). 
% edge_ex2(d, c). 
% edge_ex2(e, d).

% graph2
edge_ex2(a, b). 
edge_ex2(b, e). 
edge_ex2(c, a). 
edge_ex2(d, c). 
edge_ex2(e, d).
edge_ex2(c, e).
edge_ex2(e, a).
edge_ex2(c, b).

is_edge_ex2(X, Y) :-
    edge_ex2(X, Y);
    edge_ex2(Y, X).

% The euler/3 predicate
% euler(NumberOfEdges, Source, Result) finds an eulerian cycle inside the given graph of NumberOfEdges edges or returns false
euler(NumberOfEdges, Source, Result) :-
    euler_path(NumberOfEdges, Source, Source, [], Result).

euler_path(0, _, _, _, []) :-
    !.
euler_path(NumberOfEdges, Source, Target, Visited, [[Source, Neighbour]|Result]) :-
    NewNumberOfEdges is NumberOfEdges - 1,
    is_edge_ex2(Source, Neighbour),
    not(member([Source, Neighbour], Visited)),
    not(member([Neighbour, Source], Visited)),
    euler_path(NewNumberOfEdges, Neighbour, Target, [[Source, Neighbour]|Visited], Result).

% ========================================================================= EXERCISE 3

edge_ex3(a,b). 
edge_ex3(a,c). 
edge_ex3(c,e). 
edge_ex3(e,a). 
edge_ex3(b,d). 
edge_ex3(d,a).

is_edge_ex3(X, Y) :- % [NOTE] Directed graph
    edge_ex3(X, Y).

% The cycle/2 predicate
% cycle(Source, Result) finds all the cycles starting in Source
cycle(Source, Result) :-
    path_ex3(0, Source, Source, [], Result).

path_ex3(1, Target, Target, _, [Target]) :-
    !.
path_ex3(_, Source, Target, Visited, [Source|Result]) :-
    is_edge_ex3(Source, Neighbour),
    not(member(Source, Visited)),
    path_ex3(1, Neighbour, Target, [Source|Visited], Result).

% ========================================================================= EXERCISE 4

neighb_ex4(a, [b, c]). 
neighb_ex4(b, [d]). 
neighb_ex4(c, [e]). 
neighb_ex4(d, [a]). 
neighb_ex4(e, [a]). 

% The cycle_neighb/2 predicate
% cycle_neighb(Source, Result) finds all the cycles starting in Source
cycle_neighb(Source, Result) :-
    path_ex4(0, Source, Source, [], Result).

path_ex4(1, Target, Target, _, [Target]) :-
    !.
path_ex4(_, Source, Target, Visited, [Source|Result]) :-
    neighb_ex4(Source, NeighbourList),
    path_ex4_helper(Source, Target, NeighbourList, Visited, Result).
path_ex4_helper(Source, Target, [Neighbour|Rest], Visited, Result) :-
    not(member(Source, Visited)),
    path_ex4(1, Neighbour, Target, [Source|Visited], Result),
    path_ex4_helper(Source, Target, Rest, Visited, Result).

% ========================================================================= EXERCISE 5
% ========================================================================= EXERCISE 6
% ========================================================================= EXERCISE 7
% ========================================================================= EXERCISE 8