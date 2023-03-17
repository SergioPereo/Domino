:-dynamic have/2,end_left/1,end_right/1,played/2,pool/2,pieces_opponent/1.

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

pieces_opponent(7).


max(A,B,Max):-
    A>B,Max is A,!.
max(_,B,B).

min(A,B,Min):-
    A<B,Min is A,!.
min(_,B,B).


opponent_play:-
    pieces_opponent(X),
    Y is X-1,
    retract(pieces_opponent(X)),
    assertz(pieces_opponent(Y)).

opponent_steal:-
    pieces_opponent(X),
    Y is X+1,
    retract(pieces_opponent(X)),
    assertz(pieces_opponent(Y)).

own_steal(Left, Right):-
    assertz(have(Left,Right)),
    retract(pool(Left,Right)).

take([]):-!.
take([Piece|L]):-
    nth0(0,Piece,Left),
    nth0(1,Piece,Right),
    Left>Right,
    assertz(have(Right, Left)),
    retract(pool(Right, Left)),
    take(L),
    !.
take([Piece|L]):-
    nth0(0,Piece,Left),
    nth0(1,Piece,Right),
    assertz(have(Left, Right)),
    retract(pool(Left, Right)),
    take(L).

%Pieces in pool
pieces_left(Pieces):-
    findall([Left,Right], (pool(Left,Right)),Pieces).
%Function that changes the state of the game with a play
%0: my turn, else opponent turn
play(Left,Right,0,Turn):-
    end_left(Left),
    retract(end_left(Left)),
    assertz(end_left(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 ->
    retract(have(Left, Right))
    ;retract(pool(Left, Right)),opponent_play),!.
play(Left,Right,0,Turn):-
    retract(end_left(Right)),
    assertz(end_left(Left)),
    assertz(played(Left,Right)),
    (Turn is 0 ->
    retract(have(Left, Right))
    ;retract(pool(Left, Right)),opponent_play).
play(Left,Right,1,Turn):-
    end_right(Right),
    retract(end_right(Right)),
    assertz(end_right(Left)),
    assertz(played(Left,Right)),
    (Turn is 0 -> retract(have(Left, Right));
    retract(pool(Left, Right)),opponent_play),!.
play(Left,Right,1,Turn):-
    retract(end_right(Left)),
    assertz(end_right(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 -> retract(have(Left, Right));
    retract(pool(Left, Right)),opponent_play).
play(Left, Right, -1,Turn):-
    assertz(end_left(Left)),
    assertz(end_right(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 -> retract(have(Left, Right));
    retract(pool(Left, Right)),opponent_play).

unplay(Left,Right,0,Turn):-
    end_left(Right),
    retract(end_left(Right)),
    assertz(end_left(Left)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ; assertz(pool(Left,Right)),opponent_steal),!.
unplay(Left,Right,0,Turn):-
    retract(end_left(Left)),
    assertz(end_left(Right)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ; assertz(pool(Left,Right)),opponent_steal).
unplay(Left,Right,1,Turn):-
    end_right(Right),
    retract(end_right(Right)),
    assertz(end_right(Left)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ; assertz(pool(Left,Right)),opponent_steal),!.
unplay(Left,Right,1,Turn):-
    retract(end_right(Left)),
    assertz(end_right(Right)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ; assertz(pool(Left,Right)),opponent_steal).

can_i_place_on(Left,Right,End):-
    have(Left,Right),
    (end_left(Left);end_left(Right)),
    End=0.
can_i_place_on(Left,Right,End):-
    have(Left,Right),
    (end_right(Left);end_right(Right)),
    End=1.

can_opponent_place_on(Left,Right,End):-
    not(pieces_opponent(0)),
    pool(Left,Right),
    (end_left(Left);end_left(Right)),
    End=0.
can_opponent_place_on(Left,Right,End):-
    not(pieces_opponent(0)),
    pool(Left,Right),
    (end_right(Left);end_right(Right)),
    End=1.

opponent_possible_plays(Pieces):-
    not(end_left(_)),
    findall([Left,Right],pool(Left,Right),Pieces),!.
opponent_possible_plays(Pieces):-
    setof([Left, Right,End], (pool(Left,Right),can_opponent_place_on(Left,Right,End)),Pieces),!.
opponent_possible_plays([]).

%My possible plays
possible_plays(Pieces):-
    not(end_left(_)),
    findall([Left,Right], have(Left,Right),Pieces),!.
possible_plays(Pieces):-
    setof([Left,Right,End], (have(Left,Right),can_i_place_on(Left,Right,End)),Pieces),!.
possible_plays([]).

heuristic(1).

%End: 0,1
%alphabeta(Left,Right,End,Depth,MaxDepth,Alpha,Beta,Turn,Heuristic).
%Reach Max Depth
alphabeta(Depth,Depth,_,_,_,Heuristic,_,_,_):-
    heuristic(Heuristic),
    write("Heuristic "),write(Heuristic),nl,
    !.
alphabeta(_,_,_,_,_,Heuristic,_,_,_):-
    pieces_opponent(P),
    (not(have(_,_)),P>0->
    Heuristic=1
    ;(have(_,_),!,P==0->
     Heuristic=(-1)
     ;(possible_plays(Pieces),
      opponent_possible_plays(OpponentPieces),
       Pieces==[],
       OpponentPieces==[]->Heuristic=0;false))),write("Leaf"),nl,!.
%Win,Lose,Draw
alphabeta(Depth,MaxDepth,Alpha,Beta,0,Heuristic,DoLeft,DoRight,DoEnd):-
    possible_plays(Plays),
    NewDepth is Depth+1,
    for_each_play(Plays,0,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,NewDepth,MaxDepth),
    Heuristic=ActualMax,
    write("Check my plays"),nl,
    !.
alphabeta(Depth,MaxDepth,Alpha,Beta,1,Heuristic,DoLeft,DoRight,DoEnd):-
    opponent_possible_plays(Plays),
    NewDepth is Depth+1,
    for_each_play(Plays,1,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,NewDepth,MaxDepth),
    Heuristic=ActualMax,
    write("Check opponent plays"),nl,
    !.

for_each_play([],0,_,_,_,Alpha,_,Alpha,_,_):-!.
for_each_play([[Left,Right,End|_]|U],0,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,Depth,MaxDepth):-
    play(Left,Right,End,0),
    alphabeta(Depth,MaxDepth,Alpha,Beta,1,Heuristic,_,_,_),
    max(Alpha,Heuristic,NewAlpha),
    unplay(Left,Right,End,0),
    (Beta>NewAlpha->
    for_each_play(U,0,DoLeft,DoRight,DoEnd,NewAlpha,Beta,ActualMax,Depth,MaxDepth),
    (var(DoLeft),ActualMax is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true)
    ;(var(DoLeft),NewAlpha is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true),ActualMax=NewAlpha).

for_each_play([],1,_,_,_,_,Beta,Beta,_,_):-!.
for_each_play([[Left,Right,End|_]|U],1,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,Depth,MaxDepth):-
    play(Left,Right,End,1),
    alphabeta(Depth,MaxDepth,Alpha,Beta,0,Heuristic,_,_,_),
    min(Beta,Heuristic,NewBeta),
    unplay(Left,Right,End,1),
    (NewBeta>Alpha->
    for_each_play(U,1,DoLeft,DoRight,DoEnd,Alpha,NewBeta,ActualMax,Depth,MaxDepth),
    (var(DoLeft),ActualMax is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true)
    ;(var(DoLeft),NewBeta is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true),ActualMax=NewBeta).


choose(Heuristic,DoLeft,DoRight,DoEnd):-
    alphabeta(0,5,-2,2,0,Heuristic,DoLeft,DoRight,DoEnd).













