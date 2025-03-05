gcd(A, A, A).
gcd(A, B, X) :- 
	A > B,
	NewA is A - B,
	gcd(NewA, B, X), !.
gcd(A, B, X) :-
	%B > A,
	NewB is B - A,
	gcd(A, NewB, X), !.