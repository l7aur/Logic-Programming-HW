% The member/2 predicate
% member(element, set)
member(X, [X|_]).
member(X, [_|XS]) :- member(X, XS).

% The reunion/3 predicate
% union(set A, set B, result set)
	% iteration 1
	% union([X|XS], Y, [X|RS]) :- not(member(X, Y)), union(XS, Y, RS).
	% union([X|XS], Y, RS) :- member(X, Y), union(XS, Y, RS).
	% union([], YS, YS).
	
	% iteration 2
	union([X|XS], Y, R) :- member(X, Y), !, union(XS, Y, R).
	union([X|XS], Y, [X|RS]) :- union(XS, Y, RS).
	union([], YS, YS).
	
% The intersection/3 predicate
% intersection(set A, set B, result set)
	% iteration 1
	% intersection([X|XS], Y, [X|RS]) :- member(X, Y), intersection(XS, Y, RS).
	% intersection([X|XS], Y, RS) :- not(member(X, Y)), intersection(XS, Y, RS).
	% intersection([], _, []).
	
	% iteration 2
	intersection([X|XS], Y, [X|RS]) :- member(X, Y), !, intersection(XS, Y, RS).
	intersection([_|XS], Y, RS) :- intersection(XS, Y, RS).
	intersection([], _, []).
	
% The difference/3 predicate
% difference(set A, set B, result set)
	% iteration 1
	% difference([X|XS], Y, [X|RS]) :- not(member(X, Y)), difference(XS, Y, RS).
	% difference([X|XS], Y, RS) :- member(X, Y), difference(XS, Y, RS).
	% difference([], _, []).
	
	% iteration 2
	difference([X|XS], Y, RS) :- member(X, Y), !, difference(XS, Y, RS).
	difference([X|XS], Y, [X|RS]) :- difference(XS, Y, RS).
	difference([], _, []).