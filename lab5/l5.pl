% ilaese-8@student.ltu.se 

%state([room 1],[room 2],[room 3],robot(room number,[items]))

% Action 1: move from room 1 to room 2
action(state(R1,R2,R3,robot(1,I)),NextState,Action) :-
	member(steel,I), 	/* validate that robot has a steel key */
	Action = move(2),	/* register which action has been taken */
	NextState 		/* newly generated state */
		= state(R1,R2,R3,robot(2,I)).

% Action 2: move from room 1 to room 3
action(state(R1,R2,R3,robot(1,I)),NextState,Action) :-
	member(brass,I), 
	Action = move(3),	
	NextState 			
		= state(R1,R2,R3,robot(3,I)).

% Action 3: move from room 2 to room 1
action(state(R1,R2,R3,robot(2,I)),NextState,Action) :-
	member(steel,I), 
	Action = move(1),	
	NextState 			
		= state(R1,R2,R3,robot(1,I)).

% Action 4: move from room 3 to room 1
action(state(R1,R2,R3,robot(3,I)),NextState,Action) :-
	member(brass,I), 
	Action = move(1),	
	NextState 			
		= state(R1,R2,R3,robot(1,I)).	

% Action 5: pick up an item in room 1
action(state(R1,R2,R3,robot(1,I)),NextState,Action) :-
	length(I,Len),	  /* check if robot has room for one more item */
	Len < 2,		  
	select(X,R1,NR1), /* select an item from room 1, save new list as NR1 */
	Action = pick(X), 
	NextState 	  /* update room 1 and robot inventory */
		= state(NR1,R2,R3,robot(1,[X|I])).
	
% Action 6: pick up an item in room 2
action(state(R1,R2,R3,robot(2,I)),NextState,Action) :-
	length(I,Len),
	Len < 2,		  
	select(X,R2,NR2),
	Action = pick(X), 
	NextState
		= state(R1,NR2,R3,robot(2,[X|I])).

% Action 7: pick up an item in room 3
action(state(R1,R2,R3,robot(3,I)),NextState,Action) :-
	length(I,Len),
	Len < 2,		  
	select(X,R3,NR3),
	Action = pick(X), 
	NextState
		= state(R1,R2,NR3,robot(3,[X|I])).

% Action 8: drop an item in room 1
action(state(R1,R2,R3,robot(1,I)),NextState,Action) :-
	select(X,I,NI),		/* drop item, save new inventory (NI) and item (X)*/
	Action = drop(X),
	NextState		/* add item to room 1 (R1), update new robot inventory */
		= state([X|R1],R2,R3,robot(1,NI)).

% Action 9: drop an item in room 2
action(state(R1,R2,R3,robot(2,I)),NextState,Action) :-
	select(X,I,NI),			
	Action = drop(X),
	NextState				
		= state(R1,[X|R2],R3,robot(2,NI)).

% Action 10: drop an item in room 3
action(state(R1,R2,R3,robot(3,I)),NextState,Action) :-
	select(X,I,NI),			
	Action = drop(X),
	NextState				
		= state(R1,R2,[X|R3],robot(3,NI)).

% Goal state
solveR(state(_,R2,_,_),_,[]) :- member(package,R2). 
	
% General case
solveR(State,N,[Action|Trace]) :-
    N > 0,
    action(State,NextState,Action),
    solveR(NextState,N-1,Trace).

% Display the traced solution
display(Trace) :-
	length(Trace,L),
	write("\nA solution was found! Actions performed: "), write(L), write("\n\n"),
	trace(1,Trace).

% Trace individual steps
trace(_,[]).
trace(N,[H|T]) :- 
	write("Action "), write(N), write(": "),
	cases(H),
	nl,
	N1 is N+1,
	trace(N1,T).
	
cases(pick(steel)) :-
	write("pick up steel key").
	
cases(pick(brass)) :-
	write("pick up brass key").
	
cases(pick(Item)) :-
	write("pick up "), write(Item).

cases(move(Room)) :-
	write("move to room "), write(Room).

cases(drop(steel)) :-
	write("drop steel key").
	
cases(drop(brass)) :-
	write("drop brass key").
	
cases(drop(Item)) :-
	write("drop "), write(Item).

% Solve and display
solve() :-
	solveR(state([steel],[brass],[package],robot(1,[])),12,Trace),
	display(Trace), !.
