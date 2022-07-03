del(L, [H | T], [H | R1]):- L \= H, del(L, T, R1).
del(L, [L | T], [R1]):- del(L, T, R1).

in(L, [L | _]):- !.
in(L, [H | T]):- L \= H, in(L, T).

%gonextState([Hl | Tl], [Nl | _]):-!.

countNeighbours(_, [], 0):- !.
countNeighbours(L, [H | T], R):- countNeighbours(L, T, R1), isNeigbours(L, H), R is R1 + 1, !.
countNeighbours(L, [H | T], R):- countNeighbours(L, T, R), !.

isNeigbours(live(X, Y), live(X1, Y1)):- 1 is abs(X - X1), 0 is abs(Y - Y1).
isNeigbours(live(X, Y), live(X1, Y1)):- 0 is abs(X - X1), 1 is abs(Y - Y1).
isNeigbours(live(X, Y), live(X1, Y1)):- 1 is abs(X - X1), 1 is abs(Y - Y1).

liveInNext(L, Q):- countNeighbours(L, Q, R), 3 is R, !.
liveInNext(L, Q):- countNeighbours(L, Q, R), 2 is R, !.

change([H | T], [T | H]).

goToNext([], _, []):- !.
goToNext([H | T], Q, [H | R]):- liveInNext(H, Q), goToNext(T, Q, R), !.
goToNext([H | T], Q, R):- goToNext(T, Q, R), !.

allNeighbours(live(X, Y), R):- Xl is X - 1, Xr is X + 1, Yu is Y - 1, Yd is Y + 1,
			R = [live(Xl, Yu), live(Xl, Y), live(Xl, Yd), live(X, Yu), live(X, Yd), live(Xr, Yu),live(Xr, Y), live(Xr, Yd)], !.

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

allAdding([], []).
allAdding([H | T], R):- allAdding(T, NR), allNeighbours(H, N), concat(NR, N, R).

%add(List, Q, R)

next(Q, R):- goToNext(Q, Q, Rs), allAdding(Q, RA), goToNext(RA, Q, RS), concat(Rs, RS, R), !.

count([], 0).
count([_ | T], R):- count(T, TR), R is TR + 1.

%addNext(_, R):- count(R, S), 8 is S, !.
%addNext(live(X, Y), [R):- X1 is X + 1, Y1 is Y, not(in(live(X1, Y1), R)),

%countNeighbours(live(2, 2), [live(1, 1), live(0, 0), live(2,3)], R).
%isNeigbours(live(1, 1), live(2, 3)).
%goToNext([live(1, 1), live(0, 0),  live(2, 3), live(0, 1), live(1, 0)], [live(1, 1), live(0, 0), live(2, 3), live(0, 1), live(1, 0)], R).
mySolve([], T, T):- !.
mySolve([], [], live(0, 0)):- !.
		
