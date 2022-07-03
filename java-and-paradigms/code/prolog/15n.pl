%public functions
indexOf([E | _], E, 1) :- !.
indexOf([H | T], E, N) :- H \= E, indexOf(T, E, R), !, N is R + 1, !.

getElement([E | _], E, 1) :- !.
getElement([H | T], E, N) :- N > 1, N1 is N - 1, getElement(T, E, N1), !.

remove(H, [H | T], T) :- !.
remove(X, [], []) :- !.
remove(X, [H | T], [H | R]) :- remove(X, T, R), !.

removeAt(H, [H | T], 1, T) :- !.
removeAt(X, [H | T], N, [H | R]) :- N > 1, N1 is N - 1, removeAt(X, T, N1, R), !.

insertAt(X, L, N, R) :- removeAt(X, R, N, L).

%-------------------

%Moves--------------

validRight(N) :- R is mod(N, 4), R \= 0.
validLeft(N) :- R is mod(N, 4), R \= 1.
validUp(N) :- N > 4.
validDown(N) :- N < 13.

left(L, Rl) :-
 indexOf(L, 0, I),
 validLeft(I),
 I1 is I - 1,
 getElement(L, E2, I1),
 remove(0, L, L1), remove(E2, L1, L2),
 insertAt(0, L2, I1, R0), insertAt(E2, R0, I, Rl).
%left([1, 2, 3, 4, 5, 6,7 , 0, 8, 9, 10, 11, 12, 13,14, 15], R). 

right(L, Rl) :-
	indexOf(L, 0, I),
 	validRight(I),
 	I1 is I + 1,
 	getElement(L, E2, I1),
 	remove(0, L, L1), remove(E2, L1, L2),
 	insertAt(E2, L2, I, R0), insertAt(0, R0, I1, Rl).
 %right([1, 2, 3, 4, 5, 6, 0 , 7, 8, 9, 10, 11, 12, 13,14, 15], R). 

up(L, Rl) :-
	indexOf(L, 0, I),
 	validUp(I),
 	I1 is I - 4,
 	getElement(L, E2, I1),
 	remove(0, L, L1), remove(E2, L1, L2),
 	insertAt(0, L2, I1, R0), insertAt(E2, R0, I, Rl).
%up([1, 2, 3, 4, 5, 6,7 , 0, 8, 9, 10, 11, 12, 13,14, 15], R).  	

down(L, Rl) :-
	indexOf(L, 0, I),
 	validDown(I),
 	I1 is I + 4,
 	getElement(L, E2, I1),
 	remove(0, L, L1), remove(E2, L1, L2),
 	insertAt(E2, L2, I, R0), insertAt(0, R0, I1, Rl).
%down([1, 2, 3, 4, 5, 6,7 , 0, 8, 9, 10, 11, 12, 13,14, 15], R). 

move(L, N, left) :- left(L,N).
move(L, N, up) :- up(L,N).
move(L, N, right) :- right(L,N).
move(L, N, down) :- down(L,N).

%--------------------------

%Manhattan-distance--------
getX(P, X):- M is mod(P,4), M == 0, X is 4,!.
getX(P, X):- X is mod(P,4).

getY(P, Y):- M is mod(P,4), M == 0, Y is P/4, !.
getY(P, Y):- Y is floor(P/4)+1.

way(A, B, D) :- D is abs(A - B).

mandist(X, Y, X1, Y1, D):-
  way(X, X1, Dx),
  way(Y, Y1, Dy),
  D is Dx + Dy.

mandP(I1, I2, D):-
  getX(I1, X1),
  getY(I1, Y1),
  getX(I2, X2),
  getY(I2, Y2),
  mandist(X1, Y1, X2, Y2, D).

md([],_,_,0).
md([0 | T], L, A, D1):- md(T, L, A, D1).
md([H | T], L, A , D):- indexOf(L, H, I1), indexOf(A, H, I2), mandP(I1, I2, D1) , md(T, L, A, D2), D is D1 + D2.

hFunction(L, A, D):- md(L, L, A, D), !.
%hFunction([1, 2, 3, 4, 5, 6,7 , 0, 8, 9, 10, 11, 12, 13,14, 15],  [1,2,3,4,5,6,7,11,8,9,10,12,0,13,14,15], D).  
%--------------------------


fFunction(State, Goal, Bc, R):- 
  hFunction(State, Goal, Hn),
  R is Bc + Hn, !.

insertAll([],Open,Open) :- !.
insertAll([C|Cs],Open1,Open3):- 
  insert(C,Open1,Open2),
  insertAll(Cs,Open2,Open3), !.

insert(B,[],[B]) :- !.
insert(B,[C|R],[B,C|R]):-checkCost(B,C),!.
insert(B,[B1|R],[B1|S]):-insert(B,R,S),!.

checkCost((_,F1,_) , (_,F2,_)) :- F1 < F2, !.

successor((State, F, M), Close, Childrens, Goal) :-
  findall((Child, F, [Move | M]),
        (move(State, Child, Move), not(member(Child, Close)), fFunction(Child, Goal, F, D1)),
        Childrens).
%successor(([1,2,3,4,5,6,7,8,9,10,11,12,0,13,14,15], 0, []), [[1,2,3,4,5,6,7,8,0,10,11,12,9,13,14,15]], S, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0]).

find([(Goal, F, MoveList) | _], Close, MoveList, Goal) :- !.
find([(State, F, M) | T], Close, MoveList, Goal) :- 
	successor((State, F, M), Close, Children, Goal),
	insertAll(Children, T, R),
	find(R, [State | Close], MoveList, Goal), !.

ans(In, Out) :-
	find([(In, 0, [])], [], R, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0]),reverse(R, Out).

%ans([1, 2,3,4,5,6,0,8,9,10,7, 11,13,14,15,12], L). - долго
%ans([0, 1,3,4,5,2,6,8,9,10,7, 11,13,14,15,12], L). - дост долго
%ans([9, 0, 11, 4,5,15, 7, 3, 6, 12, 13, 1, 2,14, 8, 10], L). - слишком долго
%ans([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 14, 15], L). - решает))