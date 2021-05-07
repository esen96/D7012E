% ilaese-8@student.ltu.se

% Generates all possible sets from the original list and a list of all possible index-spans
generateSets(_,[],[]).
generateSets(L,[(I,J)|T],[(R,I,J,Sum)|Sets]) :- cut(L,I,J,R), sum(R,Sum), generateSets(L,T,Sets).

% Creates a sublist of a list by cutting it from given first- and last indices
cut([H|_],1,1,[H]).						 % I & J = 1, return head.
cut([_|T],I,J,R):- I1 is I-1, J1 is J-1, cut(T,I1,J1,R).	 % Chop heads until I = 1
cut([H|T],I,J,[H|L]):- 1 is I, K is J-1, cut(T,1,K,L).		 % Recursively append heads from case J = 1

% Gives the sum of all numbers in a list
sum([],0).
sum([H|T], S) :- sum(T,Next), S is (H+Next).

% Returns the length of a list 
len([],0).
len([_|T],R) :- len(T,L), R is L+1. 

% Generates all indices, R is a list of index tuples
generateIndices(First,Last,R) :- findall((I,J), (span(First,Last,I), span(First,Last,J), I =< J), R).

% Generates numerical spans between given From and To values
span(From,To,R) :- From =< To, R = From. 	% set From = R.
span(From,To,R) :- 				% Backtracking
	From < To, Next is From+1, 
	span(Next, To, R). 

% Insertion Sort modified to fit a list of sets
insert_sort(Sets,R):- i_sort(Sets,[],R).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,R):-insert(H,Acc,NAcc),i_sort(T,NAcc,R).

insert(X,[],[X]).
insert((Sub1,I1,J1,X),[(Sub2,I2,J2,Y)|T],[(Sub2,I2,J2,Y)|NT]):-X>Y,insert((Sub1,I1,J1,X),T,NT), !.
insert((Sub1,I1,J1,X),[(Sub2,I2,J2,Y)|T],[(Sub1,I1,J1,X),(Sub2,I2,J2,Y)|T]):-X=<Y.

% Display header 
header(L) :-
	write('\n\n'), write("Entire list: "), 
	write(L), write('\n\n'),
	write(size), write('\t'), 
	write(i), write('\t'),  
	write(j), write('\t'), 
	write(sublist), write('\n').
	
% Display the k smallest sets
display(_,0).
display([(Set,I,J,Sum)|T], K) :- 
	write(  Sum), write('\t'), 
	write(I), write('\t'), 
	write(J), write('\t'), 
	write(Set), write('\n'), 
	Next is K-1, display(T,Next).

% Main function
kSmallestSets(L,K) :- 
    len(L,Len), 
    generateIndices(1,Len,Indices), 
    generateSets(L,Indices,PS), 
    insert_sort(PS, Sorted),
    header(L),
    display(Sorted,K). 

func(X,R) :- R is (X * ((-1)**X)).
storaListan(R) :- findall(X,between(1,100,X),Y), maplist(func,Y,R).
test0() :- kSmallestSets([-1,2,-3,4,-5],3).
test1() :- storaListan(SL), kSmallestSets(SL,15).
test2() :- kSmallestSets([24,-11,-34,42,-24,7,-19,21],6).
test3() :- kSmallestSets([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3],8).
