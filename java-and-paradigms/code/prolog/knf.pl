% :NOTE: conjunction а не konjunction
// :NOTE: # Не работает для and(a, a)
makeKnf(and(E, 0), 0) :- !.
makeKnf(and(E, 1), K) :- makeKnf(E, 1), !.

makeKnf(or(E, 1), 1) :- !.
makeKnf(or(E, 0), K) :- makeKnf(E, K), !.

 makeKnf(not(not(E)), K) :- makeKnf(E, K), !.
 makeKnf(not(or(E1, E2)), K) :- makeKnf(and(not(E1), not(E2)), K), !.
makeKnf(not(and(E1, E2)), K) :- makeKnf(or(not(E1), not(E2)), K), !.

makeKnf(and(E1, E2), and(K1, K2)) :- makeKnf(E1, K1), makeKnf(E2, K2), !.

makeKnf(or(E1, E2), and(K112, K122)) :- 
	makeKnf(E1, and(K11, K12))
	makeKnf(E2, K2), 
	makeKnf(or(K11, K2), K112), 
	makeKnf(or(K12, K2), K122), !.
	
makeKnf(or(E1, E2), and(K121, K122)) :-
	makeKnf(E1, K1),
	makeKnf(E2, and(K21, K22)), 
	makeKnf(or(K1, K21), K121), 
	makeKnf(or(K1, K22), K122), !.
	
makeKnf(or(E1, E2), or(K1, K2)) :- makeKnf(E1, K1), makeKnf(E2, K2), !.

makeKnf(1, 1) :- !.
makeKnf(0, 0) :- !.

makeKnf(E, E) :- !.