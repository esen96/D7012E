/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student user id  : ilaese-8@student.ltu.se
%
/* ------------------------------------------------------- */

:- ensure_loaded('play.pl').
:- ensure_loaded('stupid.pl').
:- ensure_loaded('testboards.pl').

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

initialize(InitialState,1) :-
	initBoard(InitialState).
	
/* Check terminal state and that queried player
   has a higher score than its opponent */
winner(State,Plyr) :-
	terminal(State),
	identify(Plyr,PL/OP),
	score(PL,State,0,S1),
	score(OP,State,0,S2),
	S1 < S2.

% Check terminal state and that both players have the same score
tie(State) :-
	terminal(State),
	score(1,State,0,S1),
	score(2,State,0,S2),
	S1 = S2.

% A helper function to identify player and opponent 
identify(1,1/2).
identify(2,2/1).

% Score counts a players score by recursively counting row for row
score(_,[],Score,Score).
score(PL,[Row|T],Count,Score) :-
	countrow(PL,Row,0,RowScore),
	Count1 is Count + RowScore,
	score(PL,T,Count1,Score), !.

countrow(_,[],R,R).
countrow(PL,[H|T],Count,R) :-
	PL = H,
	Count1 is Count + 1,
	countrow(PL,T,Count1,R), !.
countrow(PL,[H|T],Count,R) :-
	PL \= H,
	countrow(PL,T,Count,R).

% Check that the only possible moves from both players are null moves 
terminal(State) :-
	moves(1,State,M1),
	moves(2,State,M2),
	M1=[n],
	M2=[n].

% Predefined utility function 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

/* We find all player moves by checking every 
   possible direction for a legal move and
   collecting them in a (sorted) set */
moves(Plyr,State,MvList) :- 
	identify(Plyr,PL/OP),
	setof(X,findmove(State,PL,OP,X),MvList),!.

% Account for terminal state
moves(_,_,[n]).

% Find a player move
findmove(State,PL,OP,R) :-
	get(State,Pos,PL),	      % get a position occupied by player
	between(1,8,Dir),	      % pick a valid direction (1-8 clockwise) 
	move(Dir,State,PL,OP,Pos,R).  % search for a move
	
/* A move is found in any direction by looking two squares ahead, 
   ensuring that we are within bounds (0-5), and that there is an enemy
   disc one move ahead to reverse. If another enemy disc is two moves
   ahead, continue recurring, if empty then we have a valid move. */

% Find northbound move 
move(1,State,PL,OP,[X,Y],R) :-
	Y1 is Y - 1, 	 		% One square north
	Y2 is Y - 2, 	 		% Two squares north
	between(0,5,Y2), 		% Check that look-ahead squares are within bounds
	get(State,[X,Y1],OP),		% Check that adjacent square is reversable
	scoutvertical(1,State,PL,OP,X,Y1,Y2,R).

% Find north-eastbound move 
move(2,State,PL,OP,[X,Y],R) :-
	Y1 is Y - 1,			% One square north
	Y2 is Y - 2,			% Two squares north
	X1 is X + 1, 	 		% One square east
	X2 is X + 2, 	 		% Two squares east
	between(0,5,X2), 		% Check that look-ahead squares are within bounds
	between(0,5,Y2),
	get(State,[X1,Y1],OP),		% Check that adjacent square is reversable
	scoutdiagonal(2,State,PL,OP,X1,Y1,X2,Y2,R).

% Find eastbound move 
move(3,State,PL,OP,[X,Y],R) :-
	X1 is X + 1, 	 		% One square east
	X2 is X + 2, 	 		% Two squares east
	between(0,5,X2), 		% Check that look-ahead squares are within bounds
	get(State,[X1,Y],OP),		% Check that adjacent square is reversable
	scouthorizontal(3,State,PL,OP,X1,X2,Y,R).

% Find south-eastbound move 
move(4,State,PL,OP,[X,Y],R) :-
	Y1 is Y + 1,			% One square south
	Y2 is Y + 2,			% Two squares south
	X1 is X + 1, 	 		% One square east
	X2 is X + 2, 	 		% Two squares east
	between(0,5,X2), 		% Check that look-ahead squares are within bounds
	between(0,5,Y2),
	get(State,[X1,Y1],OP),		% Check that adjacent square is reversable
	scoutdiagonal(4,State,PL,OP,X1,Y1,X2,Y2,R).
	
% Find southbound move 
move(5,State,PL,OP,[X,Y],R) :-
	Y1 is Y + 1, 	 		% One square south
	Y2 is Y + 2, 	 		% Two squares south
	between(0,5,Y2), 		% Check that look-ahead squares are within bounds
	get(State,[X,Y1],OP),		% Check that adjacent square is reversable
	scoutvertical(5,State,PL,OP,X,Y1,Y2,R).

% Find south-westbound move 
move(6,State,PL,OP,[X,Y],R) :-
	Y1 is Y + 1,			% One square south
	Y2 is Y + 2,			% Two squares south
	X1 is X - 1, 	 		% One square west
	X2 is X - 2, 	 		% Two squares west
	between(0,5,X2), 		% Check that look-ahead squares are within bounds
	between(0,5,Y2),
	get(State,[X1,Y1],OP),		% Check that adjacent square is reversable
	scoutdiagonal(6,State,PL,OP,X1,Y1,X2,Y2,R).

% Find westbound move 
move(7,State,PL,OP,[X,Y],R) :-
	X1 is X - 1, 	 		% One square west
	X2 is X - 2, 	 		% Two squares west
	between(0,5,X2), 		% Check that look-ahead squares are within bounds
	get(State,[X1,Y],OP),		% Check that adjacent square is reversable
	scouthorizontal(7,State,PL,OP,X1,X2,Y,R).

% Find south-westbound move 
move(8,State,PL,OP,[X,Y],R) :-
	Y1 is Y - 1,			% One square north
	Y2 is Y - 2,			% Two squares north
	X1 is X - 1, 	 		% One square west
	X2 is X - 2, 	 		% Two squares west
	between(0,5,X2), 		% Check that look-ahead squares are within bounds
	between(0,5,Y2),
	get(State,[X1,Y1],OP),		% Check that adjacent square is reversable
	scoutdiagonal(8,State,PL,OP,X1,Y1,X2,Y2,R).

/*	
	Method for scouting functions: 
	- If furthest look-ahead square is unoccupied, we've found the move
	- If furthest look-ahead square is occupied by opponent, keep recurring
	  in the same direction  
*/
scoutvertical(_,State,_,_,X,_,Y2,[X,Y2]) :-
	get(State,[X,Y2],'.'),!.

scoutvertical(Dir,State,PL,OP,X,Y1,Y2,R) :-	
	get(State,[X,Y2],OP),
	move(Dir,State,PL,OP,[X,Y1],R).

scoutdiagonal(_,State,_,_,_,_,X2,Y2,[X2,Y2]) :-
	get(State,[X2,Y2],'.'),!.

scoutdiagonal(Dir,State,PL,OP,X1,Y1,X2,Y2,R) :-
	get(State,[X2,Y2],OP),
	move(Dir,State,PL,OP,[X1,Y1],R).

scouthorizontal(_,State,_,_,_,X2,Y,[X2,Y]) :-
	get(State,[X2,Y],'.'),!.

scouthorizontal(Dir,State,PL,OP,X1,X2,Y,R) :-	
	get(State,[X2,Y],OP),
	move(Dir,State,PL,OP,[X1,Y],R).

/* 
   To change the board when a player makes a move, we 
   find all connections from the played move to its
   other discs that make up legal moves. After finding 
   the connections, we flip all discs between them. 
*/
nextState(Plyr,[n],NewState,NewState,OP) :- identify(Plyr,_/OP).
nextState(Plyr,To,State,NewState,OP) :-
	identify(Plyr,PL/OP),
	findall(X,connect(To,State,PL,OP,X),Connections),
	flipall(State,PL,To,Connections,NewState).

% No more connections, stop flipping and return new state.
flipall(NewState,_,_,[],NewState) :- !. 

% Flip connections in Connections list recursively, carry updated state every iteration.
flipall(State,PL,To,[(Dir,[X,Y])|T],NewState) :-
	flip(Dir,State,PL,[X,Y],To,FlippedState),
	flipall(FlippedState,PL,To,T,NewState).
	
/* 
	Connections are found by using move in a "backwards" 
	manner, this way we can find all legal moves ([X,Y]) 
	and directions (Dir) from the move played.
*/
connect(To,State,PL,OP,(Dir,[X,Y])) :-
	between(0,5,X),
	between(0,5,Y),
	move(Dir,State,PL,OP,[X,Y],To),
	get(State,[X,Y],PL). % Ensure that connection goes to a square occupied by the player
	
% Stop flipping when From = To, this goes for all directions.
flip(_,FlippedState,_,X,X,FlippedState) :- !.	

% Flip discs in northbound direction.
flip(1,State,PL,[X,Y],To,FlippedState) :-
	Y1 is Y - 1,	% One step north
	set(State,UpdatedState,[X,Y1],PL),
	flip(1,UpdatedState,PL,[X,Y1],To,FlippedState).

% Flip discs in north-eastbound direction.
flip(2,State,PL,[X,Y],To,FlippedState) :-
	Y1 is Y - 1,	% One step north
	X1 is X + 1,	% One step east
	set(State,UpdatedState,[X1,Y1],PL),
	flip(2,UpdatedState,PL,[X1,Y1],To,FlippedState).
	
% Flip discs in eastbound direction.
flip(3,State,PL,[X,Y],To,FlippedState) :-
	X1 is X + 1,	% One step east
	set(State,UpdatedState,[X1,Y],PL),
	flip(3,UpdatedState,PL,[X1,Y],To,FlippedState).

% Flip discs in south-eastbound direction.
flip(4,State,PL,[X,Y],To,FlippedState) :-
	Y1 is Y + 1,	% One step south
	X1 is X + 1,	% One step east
	set(State,UpdatedState,[X1,Y1],PL),
	flip(4,UpdatedState,PL,[X1,Y1],To,FlippedState).

% Flip discs in southbound direction.
flip(5,State,PL,[X,Y],To,FlippedState) :-
	Y1 is Y + 1,	% One step south
	set(State,UpdatedState,[X,Y1],PL),
	flip(5,UpdatedState,PL,[X,Y1],To,FlippedState).

% Flip discs in south-westbound direction.
flip(6,State,PL,[X,Y],To,FlippedState) :-
	Y1 is Y + 1,	% One step south
	X1 is X - 1,	% One step west
	set(State,UpdatedState,[X1,Y1],PL),
	flip(6,UpdatedState,PL,[X1,Y1],To,FlippedState).

% Flip discs in westbound direction.
flip(7,State,PL,[X,Y],To,FlippedState) :-
	X1 is X - 1,	% One step west
	set(State,UpdatedState,[X1,Y],PL),
	flip(7,UpdatedState,PL,[X1,Y],To,FlippedState).

% Flip discs in north-westbound direction.
flip(8,State,PL,[X,Y],To,FlippedState) :-
	Y1 is Y - 1,	% One step north
	X1 is X - 1,	% One step west
	set(State,UpdatedState,[X1,Y1],PL),
	flip(8,UpdatedState,PL,[X1,Y1],To,FlippedState).

% Check that proposed move is a member of legal moves
validmove(Plyr,State,n) :- moves(Plyr,State,Moves), Moves = [n],!. 
validmove(Plyr,State,Proposed) :-
	moves(Plyr,State,Moves),
	member(Proposed,Moves).

/* 
	The heuristics is based on edge avoidance 
	for both players. Player 1 is incentivized
	by high values and player 2 by low values.
*/
h(State,100) :- winner(State,1), !.
h(State,-100) :- winner(State,2), !.
h(State,0) :- tie(State), !.
h(State,Val) :- 
	/* 
		Make a list of all edgestates on the board,
		corners will be duplicated due to ranges set 
		by the between functions in edgestate. This 
		is fine since it adds extra incentive to 
		avoid corners, and leads to better results 
		than non-duplicate counts.
	*/
	findall(Discs,edgestate(State,Discs),Edges), 
	edgeheuristic(Edges,0,Val). % Count the edge heuristics

/* 
	Edge heuristic: Given a list of edgestates,  
	disincentivise edge moves for the player by 
	worsening its heuristics when occupying edge
	squares, while improving its heuristics for 
	every edge square occupied by the opponent.
*/
edgeheuristic([],Val,Val):- !.
edgeheuristic([H|T],Count,Val) :-
	H = 1, 
	C1 is Count - 1,
	edgeheuristic(T,C1,Val), !.
edgeheuristic([H|T],Count,Val) :-
	H = 2, 
	C1 is Count + 1,
	edgeheuristic(T,C1,Val), !.
edgeheuristic([H|T],Count,Val) :-
	H \= 1, % empty square
	H \= 2,
	edgeheuristic(T,Count,Val).

% Finds the state of an edge square, i.e which player occupies it.
edgestate(State,Plyr) :-
	(X = 0, between(0,5,Y) ;
   	 Y = 0, between(0,5,X) ; 
	 X = 5, between(0,5,Y) ; 
	 Y = 5, between(0,5,X)),
	 get(State,[X,Y],Plyr).

% Bounds are set outside of the heuristics range.
lowerBound(-101).
upperBound(101).

% Predefined utilities
get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 
set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

setInList( [_|RestList], [Value|RestList], 0, Value). 
setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
