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
