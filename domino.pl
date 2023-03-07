:-dynamic have/1,end_left/1,end_right/1,played/2.

pool(0,0).
pool(0,1).
pool(0,2).
pool(0,3).
pool(0,4).
pool(0,5).
pool(0,6).
pool(1,1).
pool(1,2).
pool(1,3).
pool(1,4).
pool(1,5).
pool(1,6).
pool(2,2).
pool(2,3).
pool(2,4).
pool(2,5).
pool(2,6).
pool(3,3).
pool(3,4).
pool(3,5).
pool(3,6).
pool(4,4).
pool(4,5).
pool(4,6).
pool(5,5).
pool(5,6).
pool(6,6).


max(A,B,Max):-
    A>B,Max is A,!.
max(_,B,B).

take([]):-!.
take([Piece|L]):-
    nth0(0,Piece,Left),
    nth0(1,Piece,Right),
    Left>Right,
    assertz(have(Right, Left)),
    take(L),
    !.
take([Piece|L]):-
    nth0(0,Piece,Left),
    nth0(1,Piece,Right),
    assertz(have(Left, Right)),
    take(L).

pieces_left(Pieces):-
    findall([Left,Right], (pool(Left,Right),
                           not(have(Left,Right)),
                           not(played(Left,Right))),Pieces).

play(Left,Right,0,Turn):-
    end_left(Left),
    retract(end_left(Left)),
    assertz(end_left(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 -> retract(have(Left, Right));true).
play(Left,Right,1,Turn):-
    end_right(Right),
    retract(end_right(Right)),
    assertz(end_right(Left)),
    assertz(played(Left,Right)),
    (Turn is 0 -> retract(have(Left, Right));true).
play(Left, Right, -1,Turn):-
    assertz(end_left(Left)),
    assertz(end_right(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 -> retract(have(Left, Right));true).

choose(_):-
    !.

possible_plays(Pieces):-
    not(end_left(_)),
    findall([Left,Right], have(Left,Right),Pieces),!.
possible_plays(Pieces):-
    setof([Left,Right], (have(Left,Right),(end_left(Left);
                                            end_left(Right);
                                            end_right(Left);
                                            end_right(Right))),Pieces).

