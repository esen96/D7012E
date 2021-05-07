family(
person( tom,  fox,  date(7,may,1960),  works( bbc,  15200)),
person( ann,  fox,  date(9,may,1961),  unemployed),
[ person( pat,  fox,  date(5,may,1983),  unemployed),
   person( jim,  fox,  date(5,may,1983),  unemployed),
    person( kim,  fox,  date(5,may,1983),  unemployed),
	 person( kip,  fox,  date(8,aug,1986),  unemployed),
	  person( tim,  fox,  date(8,aug,1986),  unemployed)] ).

% 4.2
twins(C1, C2) :- family(_,_,Children), findTwins(C1,C2,Children).

findTwins(C1,C2,Children) :-
	member(C1,Children),
	member(C2,Children),
	C1 \= C2, 
	C1 = person(_,_,Date1,_),
	C2 = person(_,_,Date2,_),
	Date1 = Date2.

% 4.3
nthchild( N,Child) :-
	family( _, _, Childlist),
	nth_member(N, Childlist, Child).
	
nth_member(1,[H|_],H).
nth_member(N,[_|T],X) :- 
	Next is N-1,
	nth_member(Next,T,X).
	
% 4.7
jump(X/Y, A/B) :-
	(
	(A is X+2, B is Y+1);
	(A is X+2, B is Y-1);
	(A is X-2, B is Y+1);
	(A is X-2, B is Y-1);
	(A is X+1, B is Y+2);
	(A is X+1, B is Y-2);
	(A is X-1, B is Y+2);
	(A is X-1, B is Y-2)
	),
	member(A,[1,2,3,4,5,6,7,8]),
	member(B,[1,2,3,4,5,6,7,8]).

knightpath([H|T]) :- checkMove(H,T).

checkMove(_,[]).
checkMove(M,[H|T]) :-
	findall(X,jump(M,X),R),
	member(H,R),
	checkMove(H,T).

%knightpath([2/1,Move1,5/4,Move3,X4/8]).

% 5.1
p(1).
p(2) :- !.
p(3).

% 5.2
class(N,positive) :- N > 0, !.
class(0,zero) :- !.
class(_,negative).

% 5.3
split([],_,_).

split([H|T],[H|P],N) :-
	H >= 0, !,
	split(T,P,N).
	
split([H|T],P,[H|N]) :-
	H < 0, !,
	split(T,P,N).

% 5.4
select(Candidates,RuledOut,R) :-
	member(R,Candidates),
	not(member(R,RuledOut)).
	
% 5.5
difference([],_,[]).

difference([H|T],S2,[H|R]) :-
	not(member(H,S2)),
	difference(T,S2,R), !.
	
difference([H|T],S2,R) :-
	member(H,S2),
	difference(T,S2,R), !.
