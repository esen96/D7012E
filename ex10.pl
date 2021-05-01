parent( pam, bob).
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( pat, jim).

female( pam).            % Pam is female
female( liz).
female( ann).
female( pat).
male( tom).              % Tom is male
male( bob).
male( jim).

different(X,Y) :- \+(X=Y).

sister( X, Y)  :-        % X is a sister of Y if
   parent( Z, X),
   parent( Z, Y),        % X and Y have the same parent and
   female( X),           % X is female and
   different( X, Y).     % X and Y are different
   
predecessor( X, Z)  :-   % Rule prl: X is a predecessor of Z
   parent( X, Z).

predecessor( X, Z)  :-   % Rule pr2: X is a predecessor of Z
   parent( X, Y),
   predecessor( Y, Z).

% ?- parent(Parent,pat). 
% ?- parent(liz,Child).
% ?- parent(Parent,pat), parent(Grandparent,Parent).

% 1.3
happy(X) :- parent(X, _).
hastwochildren(X) :- parent(X,Y), sister(Y,_).

% 1.4
grandchild(X,Z) :- parent(Z,Y), parent(Y,X).

% 1.6
relatives(X,Y) :- predecessor(Z,X), predecessor(Z,Y).
relatives(X,Y) :- predecessor(X,Y).
relatives(X,Y) :- predecessor(Y,X).

% 2.2
%R = rectangle( point(-1,1), point(1,1), point(1,-1), point(-1,-1) )
%T = triangle( point(4,2),point(6,4),point(7,1) )
%C = circle ( point(0,0), Diameter )

regular(rectangle(point(D,A),point(B,A),point(B,C),point(D,C))).

% 2.6
f(1,one).
f(s(1),two).
f(s(s(1)),three).
f(s(s(s(X))),N):-f(X,N).

/* 
	a) A is directly pattern matched to two.
	b) atom two does not match atom three, false.
	c) f( s(s(s(s(s(s(1)))))), C) -> last pattern is matched -> f( s(s(s(1))), C) 
	   -> last pattern is matched again -> f(1, C) -> C = one.
	d) Every s(X) function that can yield three is returned.
*/

% 2.8
%translate(1,one).
%translate(2,two).
%translate(3,three).

conc( [], L, L).
conc( [X|L1], L2, [X|L3]) :-
conc( L1, L2, L3).

% 3.1
%L = [1,2,3,4,5,6], conc(L1,[_,_,_],L).
%L = [1,2,3,4,5,6,7,8,9], conc(L1,[_,_,_],L), conc([_,_,_],L2,L1).

% 3.2
last(Item,List) :- conc(_,[Item],List).
last2(Item,[Item]).
last2(Item,[_|List]):- last2(Item,List).

% 3.3
even([]).            
even([_|Xs]) :- odd(Xs).

odd([_|Xs]) :- even(Xs).

% 3.4
reverse([],[]).
reverse([H|T], ReversedList) :- reverse(T,R), conc(R,[H],ReversedList). 

% 3.5
palindrome(List) :- reverse(List, Rev), Rev = List.

% 3.6
shift([H|T],List2) :- conc(T,[H],List2). 

% 3.7
means(0,zero).
means(1,one).
means(2,two).
means(3,three).
means(4,four).
means(5,five).
means(6,six).
means(7,seven).
means(8,eight).
means(9,nine).

translate([],[]).
translate([H|T],List2) :- means(H,Trans), translate(T,R), conc([Trans],R,List2).

% 3.8
subset([_|T], S) :- subset(T,S).
subset([],[]).						
subset([H|T],[H|S]) :- subset(T,S). 

% sublist
sublist(S,L) :- conc(_,L2,L), conc(S,_,L2).
