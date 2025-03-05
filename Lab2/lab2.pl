% The gcd/3 predicate
% gcd(X, Y, Z) computes the greatest common divisor of X and Y and returns it in Z
	% repeated subractions
	% iteration 1
	gcd(X, X, X).
	gcd(X, Y, Z) :- 
		X > Y, 
		Diff is X - Y, 
		gcd(Diff, Y, Z).
	gcd(X, Y, Z) :- 
		X < Y, 
		Diff is Y - X, 
		gcd(X, Diff, Z).
	
	% iteration 1' - usage of pruning
	gcd3(A, A, A).
	gcd3(A, B, X) :- 
		A > B,
		NewA is A - B,
		gcd3(NewA, B, X), !.
	gcd3(A, B, X) :-
		%B > A,
		NewB is B - A,
		gcd3(A, NewB, X), !.
		
	% Euclid'a algorthm
	% iteration 2
	gcd2(X, 0, X).
	gcd2(X, Y, Z) :- 
		Y \= 0, 
		Rest is X mod Y, 
		gcd2(Y, Rest, Z).
	
	
% The factorial/2 predicate
% factorial(N, F) computes N! and returns the result in F
	% iteration 1 - backwards recursion example
	factorial_bwd(0, 1).
	factorial_bwd(N, F) :- 
		N > 0, 
		N1 is N - 1, 
		factorial_bwd(N1, F1), 
		F is N * F1.
	
	% iteration 2 - forward recursion example
	factorial_fwd(N, F) :- factorial_fwdHelper(N, 1, F).
		% The factorial2Helper/3 predicate.
		% The factorial2Helper(N, Acc, F) uses an accumulator to enable forward recursion
		factorial_fwdHelper(0, Acc, F) :- F = Acc.
		factorial_fwdHelper(N, Acc, F) :- 
			N > 0, 
			N1 is N - 1, 
			Acc1 is Acc * N, 
			factorial_fwdHelper(N1, Acc1, F).

% The lcm/3 predicate
%lcm(X, Y, R) computes the least common multiplier of X and Y and returns the result in R
lcm(X, Y, R) :- gcd(X, Y, G), R is X * Y / G. 
	
% The triangle/3 predicate
% triangle(A, B, C) returns true if A, B and C are the lenghts of a triangle
triangle(A, B, C) :- 
	Temp1 is A + B,	Temp1 >= C,
	Temp2 is A + C, Temp2 >= B,
	Temp3 is B + C, Temp3 >= A.

% The solve/4 predicate
% solve(A, B, C, X) returns in X the solutions of the equation A * X ^ 2 + B * X + C = 0
% if X1 has the same value as X2 the result is returned only once 
solve(A, B, C, X) :- 
	Delta is B * B - 4 * A * C, 
	Delta > 0, (X is (-B + sqrt(Delta)) / (2 * A); X is (-B - sqrt(Delta)) / (2 * A)).
solve(A, B, C, X) :- 
	Delta is B * B - 4 * A * C, 
	Delta = 0, X is (-B + sqrt(Delta)) / (2 * A).
	
% The power_fwd/3 predicate 
% power_fwd(X, Y, R) computes X ^ Y and returns the result in R
% uses forward recursion
power_fwd(X, Y, R) :- power_fwdHelper(X, Y, 1, R).
	% The power_fwdHelper/4 predicate
	% power_fwdHelper(X, Y, Acc, R) uses an accumulator to enable forward recursion
	power_fwdHelper(0, _, Acc, R) :- R = Acc.
	power_fwdHelper(_, 0, Acc, R) :- R = Acc.
	power_fwdHelper(X, Y, Acc, R) :- 
		Y > 0, 
		Y1 is Y - 1, 
		Acc1 is Acc * X, 
		power_fwdHelper(X, Y1, Acc1, R).

% The power_bwd/3 predicate 
% power_bwd(X, Y, R) computes X ^ Y and returns the result in R
% uses backward recursion
power_bwd(0, _, 0) :- !.
power_bwd(X, 0, 1) :- X \= 0. 
power_bwd(X, Y, R) :- 
	Y > 0, 
	Y1 is Y - 1, 
	power_bwd(X, Y1, R1), 
	R is R1 * X.

% The fib2/2 predicate
% fib2(N, F) computes the N-th Fibonacci number and returns the result in F
fib2(0, 0).
fib2(1, 1).
fib2(N, F) :- 
	N1 is N - 1, 
	N2 is N - 2, 
	fib2(N1, F1), 
	fib2(N2, F2), 
	F is F1 + F2,
	!.

% The fib/2 predicate
% fib2(N, F) computes the N-th Fibonacci number and returns the result in F
% use N > 1 instead of tree pruning in fibHelper1
fib(0, 0).
fib(1, 1).
fib(N, F) :- 
	N > 1, % or ! below
	fibHelper(N, 0, 1, F).
	
	% The fibHelper/4 predicate
	% fibHelper(N, F1, F2, R) uses an accumulator to enable forward recursion
	fibHelper(1, _, A, A).
	fibHelper(N, F1, F2, R) :- 
		N > 1, 
		N1 is N - 1, 
		Sum is F1 + F2, 
		fibHelper(N1, F2, Sum, R).
		%!. or N > 1 above
	
% The for_fwd/3 predicate
% for_fwd(S, E, R) computes the sum of the numbers between S and E - 1 and returns the result in R
for_fwd(I, N, R) :- for_fwdHelper(I, N, 0, R).
	% The for_fwdHelper/4 predicate
	% for_fwdHelper(I, N, Acc, R) uses an accumulator to enable forward recursion
	for_fwdHelper(N, N, Acc, Acc).
	for_fwdHelper(I, N, Acc, R) :-
		I < N,
		NewI is I + 1,
		NewAcc is Acc + I,
		for_fwdHelper(NewI, N, NewAcc, R).

% The for/2 predicate
% for(I, O) computes the sum of all numbers from 1 to I and returns the result in O
for(In, Out) :- forHelper(In, Out, 0).
	% The forHelper/3 predicate.
	% forHelper(I, O, Acc) uses an accumulator to enable forward recursion
	forHelper(0, Out, Out).
	forHelper(In, Out, Inter):- 
		In > 0, 
		NewIn is In - 1, 
		Intermediate is Inter + In,
		forHelper(NewIn, Out, Intermediate). 

% The for_bwd/2 predicate
% for_bwd(I, O) computes the sum of all positive numbers <= I
% uses backwards recursion
for_bwd(0, 0).
for_bwd(In, Out1) :- 
	In > 0,
	In1 is In - 1,
	for_bwd(In1, Out),
	Out1 is Out + In.
	
% The while/3 predicate
% while(L, H, O) computes the sum of all numbers between L and H - 1
while(L, H, _) :-  L > H, fail.
while(L, H, Out) :- whileHelper(L, H, 0, Out).

	% The whileHelper/4 predicate
	% whileHelper(L, H, Acc, O) uses an accumulator to enable forward recursion
	whileHelper(H, H, Acc, Acc).
	whileHelper(L, H, Acc, Out) :-
		L < H,
		NewL is L + 1,
		NewAcc is Acc + L,
		whileHelper(NewL, H, NewAcc, Out).
	
% The doWhile/3 predicate
% doWhile(L, H, O) computes the sum of all numbers between L and H and returns the result in O
doWhile(L, H, _) :- L > H, fail.
doWhile(L, H, Out) :- doWhileHelper(L, H, 0, Out).

	% The doWhileHelper/4 predicate
	% doWhileHelper(L, H, Acc, Out) uses an accumulator to enable forward recursion
	doWhileHelper(H, H, Acc, Acc1) :- Acc1 is Acc + H.
	doWhileHelper(L, H, Acc, Out) :-
		L < H,
		NewAcc is Acc + L,
		NewL is L + 1,
		doWhileHelper(NewL, H, NewAcc, Out).
	