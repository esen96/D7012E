% 1.01
my_last(X,[X]).
my_last(X,[_|T]) :- my_last(X,T).

% 1.02
nest(X,[X,_]).
nest(X,[_,_|T]) :- nest(X,T).

% 1.03
kth(K,[K|_],1).
kth(K,[_|T],I) :- J is I - 1, kth(K,T,J).

% 1.04
length1([],0).
length1([_|T],N) :- length1(T,N1), N is N1+1.

% 1.05
rev(L,R) :- helper(L,R,[]).

helper([],R,R).
helper([H|T],R,Ans) :- helper(T,R,[H|Ans]). 

% 1.06
palindrome(L) :- rev(L,R), R = L.

% 1.08
rdup([],[]).
rdup([X],[X]).
rdup([H,H|T],L) :- rdup([H|T],L).
rdup([H|T1],[H|T2]) :- rdup(T1,T2).

% 1.09
pack([H,H|T],L) :- pack([ [H|H]|T ], L).
pack( [ [H|T] | H ], L) :- pack 

% 1.14
dup([],[]).
dup([X|Xs],[X,X|Ys]) :- dup(Xs,Ys). 



