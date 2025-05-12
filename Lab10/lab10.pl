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
    not(member([Source, Neighbour], Visited)), % directed graph
    path_ex3(1, Neighbour, Target, [[Source, Neighbour]|Visited], Result).

% ========================================================================= EXERCISE 4
% oriented graph
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
path_ex4(_, Source, Target, Visited, Result) :-
    neighb_ex4(Source, NeighbourList),
    path_ex4_helper(Source, Target, NeighbourList, Visited, Result).

path_ex4_helper(Source, Target, [Neighbour|_], Visited, [Source|Result]) :-
    not(member([Source, Neighbour], Visited)), % directed graph
    path_ex4(1, Neighbour, Target, [[Source, Neighbour]|Visited], Result).
path_ex4_helper(Source, Target, [_|Rest], Visited, Result) :-
    path_ex4_helper(Source, Target, Rest, Visited, Result).

% ========================================================================= EXERCISE 5

edge_ex5(b, d). 
edge_ex5(a, b). 
edge_ex5(a, c).

:-dynamic gen_neighb/2.
:-dynamic edge_ex5/2.

% The edge_to_neighb_undirected_graph predicate
% edge_to_neighb_undirected_graph converts the edge representation of an undirected 
% graph into its neighbour representation
edge_to_neighb_undirected_graph :-
    edge_ex5(X, Y),
    gen_neighb(X, NeighbX),
    gen_neighb(Y, NeighbY),
    !,
    update_neighb(X, NeighbX, Y),
    update_neighb(Y, NeighbY, X),
    retract(edge_ex5(X, Y)),
    retract(gen_neighb(X, NeighbX)),
    retract(gen_neighb(Y, NeighbY)),
    edge_to_neighb_undirected_graph.
edge_to_neighb_undirected_graph :-
    edge_ex5(X, Y),
    gen_neighb(X, NeighbX),
    !,
    update_neighb(X, NeighbX, Y),
    update_neighb(Y, [], X),
    retract(edge_ex5(X, Y)),
    retract(gen_neighb(X, NeighbX)),
    edge_to_neighb_undirected_graph.
edge_to_neighb_undirected_graph :-
    edge_ex5(X, Y),
    gen_neighb(Y, NeighbY),
    !,
    update_neighb(X, [], Y),
    update_neighb(Y, NeighbY, X),
    retract(edge_ex5(X, Y)),
    retract(gen_neighb(Y, NeighbY)),
    edge_to_neighb_undirected_graph.
edge_to_neighb_undirected_graph :-
    edge_ex5(X, Y),
    !,
    update_neighb(X, [], Y),
    update_neighb(Y, [], X),
    retract(edge_ex5(X, Y)),
    edge_to_neighb_undirected_graph.
edge_to_neighb_undirected_graph.

% The edge_to_neighb predicate
% edge_to_neighb converts the edge representation of a directed 
% graph into its neighbour representation
edge_to_neighb :-
    edge_ex5(X, Y),
    gen_neighb(X, NeighbX),
    gen_neighb(Y, _),
    !,
    update_neighb(X, NeighbX, Y),
    retract(edge_ex5(X, Y)),
    retract(gen_neighb(X, NeighbX)),
    edge_to_neighb.
edge_to_neighb :-
    edge_ex5(X, Y),
    gen_neighb(X, NeighbX),
    !,
    update_neighb(X, NeighbX, Y),
    update_neighb(Y, [], _),
    retract(edge_ex5(X, Y)),
    retract(gen_neighb(X, NeighbX)),
    edge_to_neighb.
edge_to_neighb :-
    edge_ex5(X, Y),
    gen_neighb(Y, NeighbY),
    !,
    update_neighb(X, [], Y),
    update_neighb(Y, NeighbY, _),
    retract(edge_ex5(X, Y)),
    retract(gen_neighb(Y, NeighbY)),
    edge_to_neighb.
edge_to_neighb :-
    edge_ex5(X, Y),
    !,
    update_neighb(X, [], Y),
    update_neighb(Y, [], _),
    retract(edge_ex5(X, Y)),
    edge_to_neighb.
edge_to_neighb.

update_neighb(X, List, N) :-
    not(var(N)),
    !,
    assertz(gen_neighb(X, [N|List])).
update_neighb(X, List, _) :-
    assertz(gen_neighb(X, List)).

% ========================================================================= EXERCISE 6

edge_ex6(a, b). 
edge_ex6(b, c). 
edge_ex6(a, c). 
edge_ex6(c, d). 
edge_ex6(b, d). 
edge_ex6(d, e). 
edge_ex6(e, a). 

is_edge_ex6(X, Y) :- % directed graph 
    edge_ex6(X, Y). 

% The restricted_path_efficient/4 predicate
% restricted_path_efficient(Source, Target, Restrictions, Result) finds a path that 
% complies with the restrictions in order
restricted_path_efficient(Source, Target, Restrictions, Result) :-
    restricted_path_efficient_helper(Source, Target, Restrictions, [], Result).

restricted_path_efficient_helper(Source, Target, [Neighbour|T], Visited, [Source|Result]) :-
    is_edge_ex6(Source, Neighbour),
    not(member(Source, Visited)),
    restricted_path_efficient_helper(Neighbour, Target, T, [Source|Visited], Result).
restricted_path_efficient_helper(Source, Target, L, Visited, [Source|Result]) :-
    is_edge_ex6(Source, Neighbour),
    not(member(Source, Visited)),
    restricted_path_efficient_helper(Neighbour, Target, L, [Source|Visited], Result).
restricted_path_efficient_helper(Target, Target, [], _, [Target]).

% ========================================================================= EXERCISE 7

% graph1
edge_ex7(a, c, 7). 
edge_ex7(a, b, 10). 
edge_ex7(c, d, 3). 
edge_ex7(b, e, 1). 
edge_ex7(d, e, 2).

% graph2
% edge_ex7(a, b, 3).
% edge_ex7(a, c, 2).
% edge_ex7(c, e, 1).
% edge_ex7(a, f, 3).
% edge_ex7(f, e, 1).
% edge_ex7(g, e, 2).
% edge_ex7(g, c, 3).
% edge_ex7(b, g, 5).

is_edge_ex7(X, Y, C) :-
    edge_ex7(X, Y, C). % directed graph

:-dynamic optimal_weighted_path_result/2.


% The optimal_weighted_path/3 predicate
% optimal_weighted_path(Source, Target, Result) finds the most
% cost-effective path from Source to Target (no heuristic search, however)
optimal_weighted_path(Source, Target, R) :-
    optimal_weighted_path_helper(Source, Target, [], 0, FinalCost, R),
    % write(FinalCost), write("\n"),
    % write(R), write("\n"),
    asserta(optimal_weighted_path_result(R, FinalCost)),
    fail.

optimal_weighted_path(_, _, R) :-
    collect1(99999999999, Cost, [], R),
    write("Cost is: "),
    write(Cost).

collect1(CurrentCost, FinalCost, _, R) :-
    optimal_weighted_path_result(L, NewCost),
    NewCost < CurrentCost,
    !,
    retract(optimal_weighted_path_result(L, NewCost)),
    collect1(NewCost, FinalCost, L, R).
collect1(CurrentCost, FinalCost, A, R) :-
    optimal_weighted_path_result(L, NewCost),
    !,
    retract(optimal_weighted_path_result(L, NewCost)),
    collect1(CurrentCost, FinalCost, A, R).
collect1(Cost, Cost, A, A).

optimal_weighted_path_helper(Source, Target, Visited, Cost, FCost, [Source|R]) :-
    is_edge_ex7(Source, Neighbour, MoveCost),
    not(member([Source, Neighbour], Visited)),
    NewCost is Cost + MoveCost,
    optimal_weighted_path_helper(Neighbour, Target, [[Source, Neighbour]|Visited], NewCost, FCost, R).
optimal_weighted_path_helper(Target, Target, _, Cost, Cost, [Target]).

% ========================================================================= EXERCISE 8

% A farmer, a wolf, a goat and a cabbage must traverse from the northern bank to the
% southern bank by means of a 2-seated boat that only humans can board.

% FWGC can take up to 16 states meaning 16 nodes in a graph
% meaning 120 possible bidirectional edges out of which most of them are not valid
% 0 = north, 1 = south
% a state can be encoded as a hexadecimal number 0x0 up to 0x15

edge_ex8(0, 10).
edge_ex8(10, 2).
edge_ex8(2, 11).
edge_ex8(2, 14).
edge_ex8(14, 4).
edge_ex8(4, 13).
edge_ex8(13, 5).
edge_ex8(13, 1).
edge_ex8(1, 9).
edge_ex8(1, 11).
edge_ex8(5, 15).
edge_ex8(0, 9).

is_edge_ex8(X, Y) :-
    edge_ex8(X, Y);
    edge_ex8(Y, X).

solve_puzzle :-
    find_puzzle_path(0, 15, [], R), 
    print_steps1(R).

find_puzzle_path(Source, Target, Visited, [Source|R]) :-
    is_edge_ex8(Source, Neighbour),
    not(member(Source, Visited)),
    find_puzzle_path(Neighbour, Target, [Source|Visited], R).
find_puzzle_path(Target, Target, _, [Target]).

print_steps1([H|T]) :-
    write("Everyone is on the north bank - FWGC\n"),
    print_steps_helper(H, T),
    write("Everyone is on the suth bank - FWGC\n").

print_steps_helper(Current, [H|T]) :-
    NewCabbage is H mod 2,
    NewGoat is (H div 2) mod 2,
    NewWolf is (H div 4) mod 2,
    NewFarmer is (H div 8) mod 2,
    Cabbage is Current mod 2,
    Goat is (Current div 2) mod 2,
    Wolf is (Current div 4) mod 2,
    Farmer is (Current div 8) mod 2,
    (
        (Farmer = 0, NewFarmer = 1) ->
            write("The farmer moves to the south bank"); 
        (Farmer = 1, NewFarmer = 0) ->
            write("The farmer moves to the north bank")
    ),
    (
        (NewCabbage \= Cabbage) ->
            write(" and takes the cabbage with him!\n");
        (NewWolf \= Wolf) ->
            write(" and takes the wolf with him!\n");
        (NewGoat \= Goat) ->
            write(" and takes the goat with him!\n");
        write(" with nothing!\n")
    ),
    print_steps_helper(H, T).
print_steps_helper(_, []).

