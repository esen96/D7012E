% 7.6
maketable :-
	L = [0,1,2,3,4,5,6,7,8,9],
	member(X,L),
	member(Y,L),
	Z is X*Y,
	assert(product(X,Y,Z)),
	fail.

unmaketable :-
	L = [0,1,2,3,4,5,6,7,8,9],
	member(X,L),
	member(Y,L),
	Z is X*Y,
	retract(product(X,Y,Z)),
	fail.
	
nozero :-
	retract(product(_,_,0)),
	fail.

% 7.1*
fac(0,1) :- !.
fac(N,M) :-
	N2 is N-1,
	fac(N2,M2),
	M is M2*N.

addFs(N,Max) :-
	between(N,Max,A),
	fac(A,B),
	assertz(factorial(A,B)),
	fail;true.
	
showFs :-
	factorial(X,Y),
	write("factorial("), write(X), write(","), write(Y), write(")"), nl,
	fail;true.

rmFs(N,Max) :-
	between(N,Max,A),
	fac(A,B),
	retract(factorial(A,B)),
	fail;true.
	
% 7.8
subseq(S,[_|T]) :- subseq(S,T).
subseq([],[]).						
subseq([H|S],[H|T]) :- subseq(S,T). 

powerset(S,Set) :- findall(X,subseq(X,Set),S).

% 7.2*
parent( pam, bob). % Pam is a parent of Bob
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( pat, jim).
parent( liz, tim).
parent( ben, tim).
parent( ben, jeb).
parent( pat, ben).

allparents1(R) :- findall(X,parent(X,_),R).
allparents2(R) :- setof( Parent, Child ^ parent(Parent,Child), R).
allparents3(R) :- bagof( Parent, Child ^ parent(Parent,Child), R).

benskids1(R) :- findall(X,parent(ben,X),R).
benskids2(R) :- bagof(X,parent(ben,X),R).

jebskids1(R) :- findall(X,parent(jeb,X),R).
jebskids2(R) :- bagof(X,parent(jeb,X),R).