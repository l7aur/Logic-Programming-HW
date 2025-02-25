% The woman/1 predicate
woman(ana).
woman(sara).
woman(ema).
woman(maria).
woman(dorina).
woman(carmen).
woman(irina).

% The man/1 predicate
man(andrei).
man(george).
man(alex).
man(mihai).
man(marius).
man(sergiu).

% The parent/2 predicate
parent(maria, ana). % maria is the parent of ana
parent(george, ana). % george is the parent of ana
parent(maria, andrei).
parent(george, andrei).
parent(marius, maria).
parent(dorina, maria).
parent(mihai, george).
parent(irina, george).
parent(mihai, carmen).
parent(irina, carmen).
parent(carmen, sara).
parent(alex, sara).
parent(carmen, ema).
parent(alex, ema).

% The mother/2 predicate - based on the parent and woman predicates
% X is the mother of Y if X is a woman and X is the parent of Y
mother(X, Y) :- woman(X), parent(X, Y).

% The father/2 predicate - based on the parent and man predicates
% X is the father of Y if X is a man and X is the parent of Y
father(X, Y) :- man(X), parent(X, Y).

% The sibling/2 predicate
% X and Y are siblings if they have at least one parent in common and X is different from Y
sibling(X,Y) :- parent(Z,X), parent(Z, Y), X \= Y.

% The sister/2 predicate
% X is the sister of Y if X is a woman and X and Y are siblings
sister(X, Y) :- sibling(X, Y), woman(X).

% The aunt/2 predicate
% X is the aunt of Y if X is the sister of Z and Z is the parent of Y
aunt(X, Y) :- sister(X, Z), parent(Z, Y).

% The brother/2 predicate
% X is the brother of Y if X is a man and X and Y are siblings
brother(X, Y) :- sibling(X, Y), man(X).

% The uncle/2 predicate
% X is the uncle of Y if X is the brother of Z and Z is the parent of Y
uncle(X, Y) :- brother(X, Z), parent(Z, Y).

% The grandmother/2 predicate
% X is the grandmother of Y if X is the mother of Y and Y is the mother of X
grandmother(X, Y) :- mother(X, Z), mother(Z, Y).

% The grandfather/2 predicate
% X is the grandfather of Y if X is the father of Y and Y is the father of X
grandfather(X, Y) :- father(X, Z), father(Z, Y).

% The ancestor/2 predicate
% X is the ancestor of Y if X is linked to Y through a series of parent relationships
ancestor(sergiu, mihai).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(Z, Y), ancestor(X, Z).