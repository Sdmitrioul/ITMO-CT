init([]).

init([card(A1, A2, A3, A4) | T]) :-
  \+ card(A1, A2, A3, A4),
  member(A1, ['red', 'green', 'purple']),
  member(A2, ['oval', 'squiggle', 'diamond']),
  member(A3, ['one', 'two', 'three']),
  member(A4, ['solid', 'open', 'striped']),
  assert(card(A1, A2, A3, A4)),
  init(T).

first(card(A, _, _, _), card(A, _, _, _), card(A, _, _, _)).

first(card(A1, _, _, _), card(B1, _, _, _), card(C1, _, _, _)) :-
  A1 \= B1,
  B1 \= C1,
  A1 \= C1.
  
second(card(_, A, _, _), card(_, A, _, _), card(_, A, _, _)).

second(card(_, A2, _, _), card(_, B2, _, _), card(_, C2, _, _)) :-
  A2 \= B2,
  B2 \= C2,
  A2 \= C2.

third(card(_, _, A, _), card(_, _, A, _), card(_, _, A, _)).

third(card(_, _, A3, _), card(_, _, B3, _), card(_, _, C3, _)) :-
  A3 \= B3,
  B3 \= C3,
  A3 \= C3.

fourth(card(_, _, _, A), card(_, _, _, A), card(_, _, _, A)).

fourth(card(_, _, _, A4), card(_, _, _, B4), card(_, _, _, C4)) :-
  A4 \= B4,
  B4 \= C4,
  A4 \= C4.

only(A, B, C) :-
  \+ be(A, C, B),
  \+ be(B, A, C),
  \+ be(B, C, A),
  \+ be(C, A, B),
  \+ be(C, B, A).

find(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4)) :-
  card(A1, A2, A3, A4),
  card(B1, B2, B3, B4),
  card(C1, C2, C3, C4),
  card(A1, A2, A3, A4) \= card(B1, B2, B3, B4),
  card(A1, A2, A3, A4) \= card(C1, C2, C3, C4),
  card(B1, B2, B3, B4) \= card(C1, C2, C3, C4),
  first(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4)),
  second(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4)),
  third(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4)),
  fourth(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4)),
  assert(be(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4))),
  only(card(A1, A2, A3, A4), card(B1, B2, B3, B4), card(C1, C2, C3, C4)).
