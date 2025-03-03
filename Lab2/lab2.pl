% The gcd/3 predicate
	% iteration 1
	gcd(X, X, X).
	gcd(X, Y, Z) :- X > Y, Diff is X - Y, gcd(Diff, Y, Z).
	gcd(X, Y, Z) :- X < Y, Diff is Y - X, gcd(X, Diff, Z).
	
	% iteration 2
	gcd2(X, 0, X).
	gcd2(X, Y, Z) :- Y \= 0, Rest is X mod Y, gcd2(Y, Rest, Z).
	
	
% The factorial/2 predicate
	% iteration 1
	factorial(0, 1).
	factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.
	
	% iteration 2
	factorial2(N, F) :- factorial2Helper(N, 1, F).
	% The factorial2Helper/3 predicate.
	factorial2Helper(0, Acc, F) :- F = Acc.
	factorial2Helper(N, Acc, F) :- N > 0, N1 is N - 1, Acc1 is Acc * N, factorial2Helper(N1, Acc1, F).

% The lcm/3 predicate
lcm(X, Y, R) :- gcd(X, Y, G), R is X * Y / G. 
	
% The triangle/3 predicate
triangle(A, B, C) :- Temp1 is A + B, Temp2 is A + C, Temp3 is B + C, Temp1 >= C, Temp2 >= B, Temp3 >= A.

% The solve/4 predicate
solve(A, B, C, X) :- 
	Delta is B * B - 4 * A * C, 
	Delta > 0, (X is (-B + sqrt(Delta)) / (2 * A); X is (-B - sqrt(Delta)) / (2 * A)).
solve(A, B, C, X) :- 
	Delta is B * B - 4 * A * C, 
	Delta = 0, X is (-B + sqrt(Delta)) / (2 * A).
	
% The power_fwd/3 predicate R = X^Y
power_fwd(X, Y, R) :- power_fwdHelper(X, Y, 1, R).
	% The power_fwdHelper/4 predicate
	power_fwdHelper(0, _, Acc, R) :- R = Acc.
	power_fwdHelper(_, 0, Acc, R) :- R = Acc.
	power_fwdHelper(X, Y, Acc, R) :- Y > 0, Y1 is Y - 1, Acc1 is Acc * X, power_fwdHelper(X, Y1, Acc1, R).

% The power_bwd/3 predicate R = X^Y
power_bwd(0, _, 0) :- !.
power_bwd(X, 0, 1) :- X \= 0. 
power_bwd(X, Y, R) :- Y > 0, Y1 is Y - 1, power_bwd(X, Y1, R1), R is R1 * X.


