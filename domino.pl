:-module(domino_db,[]).
:-dynamic have/2,end_left/1,end_right/1,played/2,pool/2,pieces_opponent/1,opponent_have/2,opponent_pool/2,pieces_own/1,opponent_end_left/1,opponent_end_right/1,opponent_played/2,saved_state/1,opponent_saved_state/1.
:-use_module(library(random)).
:-use_module(library(persistency)).

:-persistent
    approximation(key:atom,wins:integer,total_plays:integer).
:-db_attach('A:/ITAM/4to Semestre/Inteligencia Artificial/Proyecto 2/approximation.journal',[]),db_sync(gc(always)).

max(A,B,Max):-
    A>B,Max is A,!.
max(_,B,B).

min(A,B,Min):-
    A<B,Min is A,!.
min(_,B,B).

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
pool_pieces(Pieces):-
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
    ;retract(pool(Left,Right)),opponent_play),!.
play(Left,Right,0,Turn):-
    retract(end_left(Right)),
    assertz(end_left(Left)),
    assertz(played(Left,Right)),
    (Turn is 0 ->
    retract(have(Left, Right))
    ;retract(pool(Left,Right)),opponent_play),!.
play(Left,Right,1,Turn):-
    end_right(Right),
    retract(end_right(Right)),
    assertz(end_right(Left)),
    assertz(played(Left,Right)),
    (Turn is 0 ->
    retract(have(Left, Right))
    ;retract(pool(Left,Right)),opponent_play),!.
play(Left,Right,1,Turn):-
    retract(end_right(Left)),
    assertz(end_right(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 ->
    retract(have(Left, Right))
    ;retract(pool(Left,Right)),opponent_play),!.
play(Left, Right, -1,Turn):-
    assertz(end_left(Left)),
    assertz(end_right(Right)),
    assertz(played(Left,Right)),
    (Turn is 0 ->
    retract(have(Left, Right))
    ;retract(pool(Left,Right)),opponent_play),!.

unplay(Left,Right,0,Turn):-
    end_left(Right),
    retract(end_left(Right)),
    assertz(end_left(Left)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ;assertz(pool(Left,Right)),opponent_steal),!.
unplay(Left,Right,0,Turn):-
    retract(end_left(Left)),
    assertz(end_left(Right)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ;assertz(pool(Left,Right)),opponent_steal),!.
unplay(Left,Right,1,Turn):-
    end_right(Right),
    retract(end_right(Right)),
    assertz(end_right(Left)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ;assertz(pool(Left,Right)),opponent_steal),!.
unplay(Left,Right,1,Turn):-
    retract(end_right(Left)),
    assertz(end_right(Right)),
    retract(played(Left,Right)),
    (Turn is 0->
    assertz(have(Left,Right))
    ;assertz(pool(Left,Right)),opponent_steal),!.

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
possible_plays([]):-!.

factorial(1,1):-!.
factorial(X,Y) :-
	X > 1,
	X1 is X-1,
	factorial(X1,Y1),
	Y is X * Y1.

combinacion(X,Y,C) :-
	factorial(X,Num),
	R is X-Y,
	factorial(R,Facr),
	factorial(Y,Facy),
	Den is Facr * Facy,
	C is Num/Den.

check_steal_plays([],Count,Count):-!.
check_steal_plays([[Left,Right,End|_]|Rest],Count,Num):-
    play(Left,Right,End,1),
    possible_plays(Plays),
    (Plays==[]->unplay(Left,Right,End,1),NewCount is Count+1,check_steal_plays(Rest,NewCount,Num)
    ;unplay(Left,Right,End,1),check_steal_plays(Rest,Count,Num)).

opponent_check_steal_plays([],Count,Count):-!.
opponent_check_steal_plays([[Left,Right,End|_]|Rest],Count,Num):-
    play(Left,Right,End,0),
    opponent_possible_plays(Plays),
    (Plays==[]->unplay(Left,Right,End,0),NewCount is Count+1,opponent_check_steal_plays(Rest,NewCount,Num)
    ;unplay(Left,Right,End,0),opponent_check_steal_plays(Rest,Count,Num)).

calculaP3(0,P3):-
    possible_plays(Plays),
    opponent_check_steal_plays(Plays,0,Num),
    length(Plays,Tam),
    (Tam==0->P3 is 0;P3 is ((Num/Tam))).
calculaP3(1,P3):-
    opponent_possible_plays(Plays),
    check_steal_plays(Plays,0,Num),
    length(Plays,Tam),
    (Tam==0->P3 is 0;P3 is (-(Num/Tam))).

calculaP2(0,0):-!.
calculaP2(1,P2):-
    opponent_possible_plays(PiecesOpp),
    length(PiecesOpp,SonsOpp),
    pool_pieces(PiecesTodo),
    length(PiecesTodo,Quedan),
    combinacion(Quedan-SonsOpp, SonsOpp, N),
    combinacion(Quedan,SonsOpp, D),
    P2 is (N/D).

check_loss_plays([],Count,Count):-!.
check_loss_plays([[Left,Right,End|_]|Rest],Count,Num):-
    play(Left,Right,End,1),
    pieces_opponent(P),
    (P==0->unplay(Left,Right,End,1),NewCount is Count+1,check_loss_plays(Rest,NewCount,Num)
    ;unplay(Left,Right,End,1),check_loss_plays(Rest,Count,Num)).

calculaP1(1,P1):-
    opponent_possible_plays(Plays),
    check_loss_plays(Plays,0,Num),
    length(Plays,Tam),
    (Tam==0->P1 is 0;P1 is (-(Num/Tam))).

calculaP1(0,0):-!.


heuristic(Turn,Vh):-
	%contar numero de hijos del nodo actual
	%estoy suponiendo que el numero de hijos del nodo es opponent_possible_plays
	calculaP1(Turn,P1),
        (P1==0->calculaP2(Turn,P2),calculaP3(Turn,P3),Vh is (P2*0.5+P3*0.5);Vh is P1).

%End: 0,1
%alphabeta(Left,Right,End,Depth,MaxDepth,Alpha,Beta,Turn,Heuristic).
%Reach Max Depth
alphabeta(Depth,Depth,_,_,Turn,Heuristic,_,_,_):-
    heuristic(Turn,Heuristic),
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
       OpponentPieces==[]->Heuristic=0;false))),!.
%Win,Lose,Draw
alphabeta(Depth,MaxDepth,Alpha,Beta,0,Heuristic,DoLeft,DoRight,DoEnd):-
    possible_plays(Plays),
    NewDepth is Depth+1,
    for_each_play(Plays,0,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,NewDepth,MaxDepth),
    Heuristic=ActualMax,
    !.
alphabeta(Depth,MaxDepth,Alpha,Beta,1,Heuristic,DoLeft,DoRight,DoEnd):-
    opponent_possible_plays(Plays),
    NewDepth is Depth+1,
    for_each_play(Plays,1,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,NewDepth,MaxDepth),
    Heuristic=ActualMax,
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
    alphabeta(0,8,-2,2,0,Heuristic,DoLeft,DoRight,DoEnd).

opponent_pool(0,0).
opponent_pool(0,1).
opponent_pool(0,2).
opponent_pool(0,3).
opponent_pool(0,4).
opponent_pool(0,5).
opponent_pool(0,6).
opponent_pool(1,1).
opponent_pool(1,2).
opponent_pool(1,3).
opponent_pool(1,4).
opponent_pool(1,5).
opponent_pool(1,6).
opponent_pool(2,2).
opponent_pool(2,3).
opponent_pool(2,4).
opponent_pool(2,5).
opponent_pool(2,6).
opponent_pool(3,3).
opponent_pool(3,4).
opponent_pool(3,5).
opponent_pool(3,6).
opponent_pool(4,4).
opponent_pool(4,5).
opponent_pool(4,6).
opponent_pool(5,5).
opponent_pool(5,6).
opponent_pool(6,6).

pieces_own(7).

opponent_opponent_play:-
    pieces_own(X),
    Y is X-1,
    retract(pieces_own(X)),
    assertz(pieces_own(Y)).

opponent_opponent_steal:-
    pieces_own(X),
    Y is X+1,
    retract(pieces_own(X)),
    assertz(pieces_own(Y)).

opponent_own_steal(Left, Right):-
    assertz(opponent_have(Left,Right)),
    retract(opponent_pool(Left,Right)).

opponent_take([]):-!.
opponent_take([[Left,Right|_]|L]):-
    Left>Right,
    assertz(opponent_have(Right, Left)),
    retract(opponent_pool(Right, Left)),
    opponent_take(L),
    !.
opponent_take([[Left,Right|_]|L]):-
    assertz(opponent_have(Left, Right)),
    retract(opponent_pool(Left, Right)),
    opponent_take(L).

opponent_can_i_place_on(Left,Right,End):-
    opponent_have(Left,Right),
    (opponent_end_left(Left);opponent_end_left(Right)),
    End=0.
opponent_can_i_place_on(Left,Right,End):-
    opponent_have(Left,Right),
    (opponent_end_right(Left);opponent_end_right(Right)),
    End=1.

opponent_can_opponent_place_on(Left,Right,End):-
    not(pieces_own(0)),
    opponent_pool(Left,Right),
    (opponent_end_left(Left);opponent_end_left(Right)),
    End=0.
opponent_can_opponent_place_on(Left,Right,End):-
    not(pieces_own(0)),
    opponent_pool(Left,Right),
    (opponent_end_right(Left);opponent_end_right(Right)),
    End=1.

opponent_opponent_possible_plays(Pieces):-
    not(opponent_end_left(_)),
    findall([Left,Right],opponent_pool(Left,Right),Pieces),!.
opponent_opponent_possible_plays(Pieces):-
    setof([Left, Right,End], (opponent_pool(Left,Right),opponent_can_opponent_place_on(Left,Right,End)),Pieces),!.
opponent_opponent_possible_plays([]).

%My possible plays
enemy_possible_plays(Pieces):-
    not(opponent_end_left(_)),
    findall([Left,Right], opponent_have(Left,Right),Pieces),!.
enemy_possible_plays(Pieces):-
    setof([Left,Right,End], (opponent_have(Left,Right),opponent_can_i_place_on(Left,Right,End)),Pieces),!.
enemy_possible_plays([]):-!.

%Pieces in pool
opponent_pool_pieces(Pieces):-
    findall([Left,Right], (opponent_pool(Left,Right)),Pieces).

%End: 0,1
%alphabeta(Left,Right,End,Depth,MaxDepth,Alpha,Beta,Turn,Heuristic).
%Reach Max Depth

enemy_check_steal_plays([],Count,Count):-!.
enemy_check_steal_plays([[Left,Right,End|_]|Rest],Count,Num):-
    opponent_play(Left,Right,End,1),
    opponent_opponent_possible_plays(Plays),
    (Plays==[]->opponent_unplay(Left,Right,End,1),NewCount is Count+1,enemy_check_steal_plays(Rest,NewCount,Num)
    ;opponent_unplay(Left,Right,End,1),enemy_check_steal_plays(Rest,Count,Num)).

opponent_opponent_check_steal_plays([],Count,Count):-!.
opponent_opponent_check_steal_plays([[Left,Right,End|_]|Rest],Count,Num):-
    opponent_play(Left,Right,End,0),
    enemy_possible_plays(Plays),
    (Plays==[]->opponent_unplay(Left,Right,End,0),NewCount is Count+1,opponent_opponent_check_steal_plays(Rest,NewCount,Num)
    ;opponent_unplay(Left,Right,End,0),opponent_opponent_check_steal_plays(Rest,Count,Num)).

opponent_calculaP3(0,P3):-
    opponent_opponent_possible_plays(Plays),
    opponent_opponent_check_steal_plays(Plays,0,Num),
    length(Plays,Tam),
    (Tam==0->P3 is 0; P3 is ((Num/Tam))).
opponent_calculaP3(1,P3):-
    enemy_possible_plays(Plays),
    enemy_check_steal_plays(Plays,0,Num),
    length(Plays,Tam),
    (Tam==0->P3 is 0;P3 is (-(Num/Tam))).

opponent_calculaP2(1,0):-!.
opponent_calculaP2(0,P2):-
    opponent_opponent_possible_plays(PiecesOpp),
    length(PiecesOpp,SonsOpp),
    opponent_pool_pieces(PiecesTodo),
    length(PiecesTodo,Quedan),
    combinacion(Quedan-SonsOpp, SonsOpp, N),
    combinacion(Quedan,SonsOpp, D),
    P2 is (-(N/D)).

opponent_check_loss_plays([],Count,Count):-!.
opponent_check_loss_plays([[Left,Right,End|_]|Rest],Count,Num):-
    opponent_play(Left,Right,End,1),
    pieces_own(P),
    (P==0->opponent_unplay(Left,Right,End,1),NewCount is Count+1,opponent_check_loss_plays(Rest,NewCount,Num)
    ;opponent_unplay(Left,Right,End,1),opponent_check_loss_plays(Rest,Count,Num)).

opponent_calculaP1(0,P1):-
    opponent_opponent_possible_plays(Plays),
    opponent_check_loss_plays(Plays,0,Num),
    length(Plays,Tam),
    (Tam==0->P1 is 0;P1 is (-(Num/Tam))).
opponent_calculaP1(1,0):-!.


opponent_heuristic(Turn,Vh):-
	%contar numero de hijos del nodo actual
	%estoy suponiendo que el numero de hijos del nodo es opponent_possible_plays
	opponent_calculaP1(Turn,P1),
        (P1==0->opponent_calculaP2(Turn,P2),opponent_calculaP3(Turn,P3),Vh is (P2*0.5+P3*0.5);Vh is P1).


opponent_alphabeta(Depth,Depth,_,_,Turn,Heuristic,_,_,_):-
    opponent_heuristic(Turn,Heuristic),
    !.
opponent_alphabeta(_,_,_,_,_,Heuristic,_,_,_):-
    pieces_own(P),
    (not(opponent_have(_,_)),P>0->
    Heuristic=(-1)
    ;(opponent_have(_,_),!,P==0->
     Heuristic=1
     ;(enemy_possible_plays(Pieces),
      opponent_opponent_possible_plays(OpponentPieces),
       Pieces==[],
       OpponentPieces==[]->Heuristic=0;false))),!.
%Win,Lose,Draw
opponent_alphabeta(Depth,MaxDepth,Alpha,Beta,0,Heuristic,DoLeft,DoRight,DoEnd):-
    opponent_opponent_possible_plays(Plays),
    NewDepth is Depth+1,
    opponent_for_each_play(Plays,0,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,NewDepth,MaxDepth),
    Heuristic=ActualMax,
    !.
opponent_alphabeta(Depth,MaxDepth,Alpha,Beta,1,Heuristic,DoLeft,DoRight,DoEnd):-
    enemy_possible_plays(Plays),
    NewDepth is Depth+1,
    opponent_for_each_play(Plays,1,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,NewDepth,MaxDepth),
    Heuristic=ActualMax,
    !.

opponent_for_each_play([],0,_,_,_,Alpha,_,Alpha,_,_):-!.
opponent_for_each_play([[Left,Right,End|_]|U],0,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,Depth,MaxDepth):-
    opponent_play(Left,Right,End,0),
    opponent_alphabeta(Depth,MaxDepth,Alpha,Beta,1,Heuristic,_,_,_),
    max(Alpha,Heuristic,NewAlpha),
    opponent_unplay(Left,Right,End,0),
    (Beta>NewAlpha->
    opponent_for_each_play(U,0,DoLeft,DoRight,DoEnd,NewAlpha,Beta,ActualMax,Depth,MaxDepth),
    (var(DoLeft),ActualMax is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true)
    ;(var(DoLeft),NewAlpha is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true),ActualMax=NewAlpha).
opponent_for_each_play([],1,_,_,_,_,Beta,Beta,_,_):-!.
opponent_for_each_play([[Left,Right,End|_]|U],1,DoLeft,DoRight,DoEnd,Alpha,Beta,ActualMax,Depth,MaxDepth):-
    opponent_play(Left,Right,End,1),
    opponent_alphabeta(Depth,MaxDepth,Alpha,Beta,0,Heuristic,_,_,_),
    min(Beta,Heuristic,NewBeta),
    opponent_unplay(Left,Right,End,1),
    (NewBeta>Alpha->
    opponent_for_each_play(U,1,DoLeft,DoRight,DoEnd,Alpha,NewBeta,ActualMax,Depth,MaxDepth),
    (var(DoLeft),ActualMax is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true)
    ;(var(DoLeft),NewBeta is Heuristic -> DoLeft=Left,DoRight=Right,DoEnd=End;true),ActualMax=NewBeta).

opponent_play(Left,Right,0,Turn):-
    opponent_end_left(Left),
    retract(opponent_end_left(Left)),
    assertz(opponent_end_left(Right)),
    assertz(opponent_played(Left,Right)),
    (Turn is 0 ->
    retract(opponent_pool(Left,Right)),opponent_opponent_play
    ;retract(opponent_have(Left, Right))),!.
opponent_play(Left,Right,0,Turn):-
    retract(opponent_end_left(Right)),
    assertz(opponent_end_left(Left)),
    assertz(opponent_played(Left,Right)),
    (Turn is 0 ->
    retract(opponent_pool(Left,Right)),opponent_opponent_play
    ;retract(opponent_have(Left, Right))),!.
opponent_play(Left,Right,1,Turn):-
    opponent_end_right(Right),
    retract(opponent_end_right(Right)),
    assertz(opponent_end_right(Left)),
    assertz(opponent_played(Left,Right)),
    (Turn is 0 ->
     retract(opponent_pool(Left,Right)),opponent_opponent_play
    ;retract(opponent_have(Left, Right))),!.
opponent_play(Left,Right,1,Turn):-
    retract(opponent_end_right(Left)),
    assertz(opponent_end_right(Right)),
    assertz(opponent_played(Left,Right)),
    (Turn is 0 ->
    retract(opponent_pool(Left,Right)),opponent_opponent_play
    ;retract(opponent_have(Left, Right))),!.
opponent_play(Left, Right, -1,Turn):-
    assertz(opponent_end_left(Left)),
    assertz(opponent_end_right(Right)),
    assertz(opponent_played(Left,Right)),
    (Turn is 0 ->
     retract(opponent_pool(Left,Right)),opponent_opponent_play
    ;retract(opponent_have(Left, Right))),!.

opponent_unplay(Left,Right,0,Turn):-
    opponent_end_left(Right),
    retract(opponent_end_left(Right)),
    assertz(opponent_end_left(Left)),
    retract(opponent_played(Left,Right)),
    (Turn is 0->
     assertz(opponent_pool(Left,Right)),opponent_opponent_steal
    ; assertz(opponent_have(Left,Right))),!.
opponent_unplay(Left,Right,0,Turn):-
    retract(opponent_end_left(Left)),
    assertz(opponent_end_left(Right)),
    retract(opponent_played(Left,Right)),
    (Turn is 0->
     assertz(opponent_pool(Left,Right)),opponent_opponent_steal
    ; assertz(opponent_have(Left,Right))),!.
opponent_unplay(Left,Right,1,Turn):-
    opponent_end_right(Right),
    retract(opponent_end_right(Right)),
    assertz(opponent_end_right(Left)),
    retract(opponent_played(Left,Right)),
    (Turn is 0->
     assertz(opponent_pool(Left,Right)),opponent_opponent_steal
    ; assertz(opponent_have(Left,Right))),!.
opponent_unplay(Left,Right,1,Turn):-
    retract(opponent_end_right(Left)),
    assertz(opponent_end_right(Right)),
    retract(opponent_played(Left,Right)),
    (Turn is 0->
     assertz(opponent_pool(Left,Right)),opponent_opponent_steal
    ; assertz(opponent_have(Left,Right))),!.

opponent_choose(Heuristic,DoLeft,DoRight,DoEnd):-
    opponent_alphabeta(0,8,-2,2,1,Heuristic,DoLeft,DoRight,DoEnd).

pick_pieces(0,_,[]):-!.
pick_pieces(Amount,Pool,[Piece|Picking]):-
    length(Pool,Len),
    Amount=<Len,
    random_select(Piece,Pool,NewPool),
    NewAmount is Amount-1,
    pick_pieces(NewAmount,NewPool,Picking),!.

mule_hands(Pieces):-
    findall([X,X],(have(X,X);opponent_have(X,X)),Pieces).

hands(Pieces):-
    findall([Left,Right],(have(Left,Right);opponent_have(Left,Right)),Pieces).

hand(Pieces):-
    findall([Left,Right],have(Left,Right),Pieces).

opponent_hand(Pieces):-
    findall([Left,Right],opponent_have(Left,Right),Pieces).

intersected_pools(Pieces):-
    findall([Left,Right],(pool(Left,Right),opponent_pool(Left,Right)),Pieces).

open_own_hand:-
    intersected_pools(Pieces),
    pick_pieces(7,Pieces,Hand),
    take(Hand).

open_opponent_hand:-
    intersected_pools(Pieces),
    pick_pieces(7,Pieces,Hand),
    opponent_take(Hand).

open_hands:-
    open_own_hand,
    open_opponent_hand.

pick_piece(0):-
    intersected_pools(Pieces),
    pick_pieces(1,Pieces,Hand),
    take(Hand),
    opponent_opponent_steal.

pick_piece(1):-
    intersected_pools(Pieces),
    pick_pieces(1,Pieces,Hand),
    opponent_take(Hand),
    opponent_steal.

largest_pieces_left([],LeftMax,LeftMax,[]):-!.
largest_pieces_left([[Left,Right|_]|U],ActualLeftMax,LeftMax,AppendedList):-
    max(Left,ActualLeftMax,NewActualLeftMax),
    largest_pieces_left(U,NewActualLeftMax,LeftMax,ActualList),
    (Left is LeftMax->AppendedList = [[Left,Right]|ActualList]
    ;AppendedList = ActualList),
    !.
largest_pieces_right([],RightMax,RightMax,[]):-!.
largest_pieces_right([[Left,Right|_]|U],ActualRightMax,RightMax,AppendedList):-
    max(Right,ActualRightMax,NewActualRightMax),
    largest_pieces_right(U,NewActualRightMax,RightMax,ActualList),
    (Right is RightMax->AppendedList = [[Left,Right]|ActualList]
    ;AppendedList = ActualList),
    !.

first_piece([[Left,Right|_]|_],Left,Right).

select_first_move(Left,Right,Turn):-
    mule_hands(Mules),
    (Mules==[]->
    hands(Pieces),
    largest_pieces_left(Pieces,-1,_,LargestLeftPieces),
    largest_pieces_right(LargestLeftPieces,-1,_,LargestPieces),
    first_piece(LargestPieces,Left,Right),
    (have(Left,Right)->Turn=0;Turn=1)
    ;
    largest_pieces_left(Mules,-1,_,LargestMules),
    first_piece(LargestMules,Left,Right),
    (have(Left,Right)->Turn=0;Turn=1)
    ).

inverse_turn(0,1).
inverse_turn(1,0).

get_keys(Keys):-
    findall(Key,saved_state(Key),Keys).

opponent_get_keys(Keys):-
    findall(Key,opponent_saved_state(Key),Keys).

store_keys([],1):-!.
store_keys([Key|Rest],1):-
    (approximation(Key,Wins,Total)->
    NewWins is Wins+1,
    NewTotal is Total+1,
    with_mutex(domino_db,
              (retract_approximation(Key,Wins,Total),
               assert_approximation(Key,NewWins,NewTotal))),
     write("It repeated"),nl
    ;assert_approximation(Key,1,1)),
    store_keys(Rest,1),
    !.
store_keys([],0):-!.
store_keys([Key|Rest],0):-
    (approximation(Key,Wins,Total)->
    NewTotal is Total+1,
    with_mutex(domino_db,
              (retract_approximation(Key,Wins,Total),
               assert_approximation(Key,Wins,NewTotal))),
     write("It repeated"),nl
    ;assert_approximation(Key,0,1)),
    store_keys(Rest,0),
    !.

store_keys([],-1):-!.
store_keys([Key|Rest],-1):-
    (approximation(Key,Wins,Total)->
    NewTotal is Total+1,
    with_mutex(domino_db,
              (retract_approximation(Key,Wins,Total),
               assert_approximation(Key,Wins,NewTotal))),
     write("It repeated"),nl
    ;assert_approximation(Key,0,1)),
    store_keys(Rest,-1),
    !.

win:-
    write("Win"),nl,
    get_keys(Keys),
    %write(Keys),nl,
    opponent_get_keys(OpponentKeys),
    %write(OpponentKeys),nl,
    store_keys(Keys,1),
    store_keys(OpponentKeys,-1),
    !.

lose:-
    write("Lose"),nl,
    get_keys(Keys),
    %write(Keys),nl,
    opponent_get_keys(OpponentKeys),
    %write(OpponentKeys),nl,
    store_keys(Keys,-1),
    store_keys(OpponentKeys,1),
    !.

draw:-
    write("Draw"),nl,
    get_keys(Keys),
    %write(Keys),nl,
    opponent_get_keys(OpponentKeys),
    %write(OpponentKeys),nl,
    store_keys(Keys,0),
    store_keys(OpponentKeys,0),
    !.

opened_game_logic(0):-
    pieces_opponent(P),
    not(have(_,_)),P>0,
    win,!.
opened_game_logic(0):-
    pieces_opponent(P),
    have(_,_),P==0,!,
    lose,!.
opened_game_logic(0):-
    possible_plays(Plays),
    enemy_possible_plays(Opponent_Plays),
    intersected_pools(Pool),
    Plays==[],
    Opponent_Plays==[],
    Pool==[],
    draw.
opened_game_logic(0):-
    possible_plays(Pieces),
    length(Pieces,Length),
    pool_pieces(Pool),
    length(Pool,PLength),
    pieces_opponent(P),
    (Length==0->(PLength==P->
    opened_game_logic(1);pick_piece(0),opened_game_logic(0));
    (Length==1->
    nth0(0,Pieces,Play),
     nth0(0,Play,Left),
    nth0(1,Play,Right),
    nth0(2,Play,End),
    %write(Left),write(", "),write(Right),write(", "),write(End),write(", "),
    %write(0),nl,
    save_state(Left,Right,End),
    play(Left,Right,End,0),
    opponent_play(Left,Right,End,0),
    opened_game_logic(1);
    choose(_,Left,Right,End),
    %write(Left),write(", "),write(Right),write(", "),write(End),write(", "),
    %write(0),nl,
    save_state(Left,Right,End),
    play(Left,Right,End,0),
    opponent_play(Left,Right,End,0),
    opened_game_logic(1))).
opened_game_logic(1):-
    pieces_own(P),
    not(opponent_have(_,_)),P>0,
    lose,!.
opened_game_logic(1):-
    pieces_own(P),
    opponent_have(_,_),P==0,!,
    win,!.
opened_game_logic(1):-
    possible_plays(Plays),
    enemy_possible_plays(Opponent_Plays),
    intersected_pools(Pool),
    Plays==[],
    Opponent_Plays==[],
    Pool==[],
    draw.
opened_game_logic(1):-
    enemy_possible_plays(Pieces),
    length(Pieces,Length),
    opponent_pool_pieces(Pool),
    length(Pool,PLength),
    pieces_own(P),
    (Length==0->(PLength==P->
    opened_game_logic(0);pick_piece(1),opened_game_logic(1));
    (Length==1->
    nth0(0,Pieces,Play),
     nth0(0,Play,Left),
    nth0(1,Play,Right),
    nth0(2,Play,End),
    %write(Left),write(", "),write(Right),write(", "),write(End),write(", "),
    %write(1),nl,
    opponent_save_state(Left,Right,End),
    play(Left,Right,End,1),
    opponent_play(Left,Right,End,1),
    opened_game_logic(0);
    opponent_choose(_,Left,Right,End),
    %write(Left),write(", "),write(Right),write(", "),write(End),write(", "),
    %write(1),nl,
    opponent_save_state(Left,Right,End),
    play(Left,Right,End,1),
    opponent_play(Left,Right,End,1),
    opened_game_logic(0))).

pieces_played(Played):-
    findall([Left,Right],played(Left,Right),Played).

opponent_pieces_played(Played):-
    findall([Left,Right],opponent_played(Left,Right),Played).

pieces_to_string([],""):-!.
pieces_to_string([A|U],String):-
    atomics_to_string(A,',',PieceString),
    pieces_to_string(U,LastString),
    (LastString==""->String=PieceString
    ;atomics_to_string([PieceString,LastString],'_',String)),
    !.

get_state(Left,Right,End,State):-
    hand(Hand),
    pieces_to_string(Hand,HandString),
    pool_pieces(Pool),
    pieces_to_string(Pool,PoolString),
    pieces_played(Played),
    pieces_to_string(Played,PlayedString),
    end_left(EL),
    end_right(ER),
    pieces_opponent(PO),
    atomics_to_string([Left,Right,End],',',PTP),
    atomics_to_string([HandString,PoolString,EL,ER,PlayedString,PO,PTP],';',State),
    !.

opponent_get_state(Left,Right,End,State):-
    opponent_hand(Hand),
    pieces_to_string(Hand,HandString),
    opponent_pool_pieces(Pool),
    pieces_to_string(Pool,PoolString),
    opponent_pieces_played(Played),
    pieces_to_string(Played,PlayedString),
    opponent_end_left(EL),
    opponent_end_right(ER),
    pieces_own(PO),
    atomics_to_string([Left,Right,End],',',PTP),
    atomics_to_string([HandString,PoolString,EL,ER,PlayedString,PO,PTP],';',State),
    !.

save_state(Left,Right,End):-
    get_state(Left,Right,End,State),
    variant_sha1(State,Key),
    assertz(saved_state(Key)).

opponent_save_state(Left,Right,End):-
    opponent_get_state(Left,Right,End,State),
    variant_sha1(State,Key),
    assertz(opponent_saved_state(Key)).

reset_game:-
    (saved_state(_),!->retractall(saved_state(_));true),
    (opponent_saved_state(_),!->retractall(opponent_saved_state(_));true),
    (have(_,_),!->retractall(have(_,_));true),
    (end_left(_)->retractall(end_left(_));true),
    (end_right(_)->retractall(end_right(_));true),
    (played(_,_),!->retractall(played(_,_));true),
    (pool(_,_),!->retractall(pool(_,_));true),
    (pieces_opponent(_)->retractall(pieces_opponent(_));true),
    (opponent_have(_,_),!->retractall(opponent_have(_,_));true),
    (opponent_pool(_,_),!->retractall(opponent_pool(_,_));true),
    (pieces_own(_)->retractall(pieces_own(_));true),
    (opponent_end_left(_)->retractall(opponent_end_left(_));true),
    (opponent_end_right(_)->retractall(opponent_end_right(_));true),
    (opponent_played(_,_),!->retractall(opponent_played(_,_));true),
    assertz(pool(0,0)),
    assertz(pool(0,1)),
    assertz(pool(0,2)),
    assertz(pool(0,3)),
    assertz(pool(0,4)),
    assertz(pool(0,5)),
    assertz(pool(0,6)),
    assertz(pool(1,1)),
    assertz(pool(1,2)),
    assertz(pool(1,3)),
    assertz(pool(1,4)),
    assertz(pool(1,5)),
    assertz(pool(1,6)),
    assertz(pool(2,2)),
    assertz(pool(2,3)),
    assertz(pool(2,4)),
    assertz(pool(2,5)),
    assertz(pool(2,6)),
    assertz(pool(3,3)),
    assertz(pool(3,4)),
    assertz(pool(3,5)),
    assertz(pool(3,6)),
    assertz(pool(4,4)),
    assertz(pool(4,5)),
    assertz(pool(4,6)),
    assertz(pool(5,5)),
    assertz(pool(5,6)),
    assertz(pool(6,6)),
    assertz(pieces_opponent(7)),
    assertz(opponent_pool(0,0)),
    assertz(opponent_pool(0,1)),
    assertz(opponent_pool(0,2)),
    assertz(opponent_pool(0,3)),
    assertz(opponent_pool(0,4)),
    assertz(opponent_pool(0,5)),
    assertz(opponent_pool(0,6)),
    assertz(opponent_pool(1,1)),
    assertz(opponent_pool(1,2)),
    assertz(opponent_pool(1,3)),
    assertz(opponent_pool(1,4)),
    assertz(opponent_pool(1,5)),
    assertz(opponent_pool(1,6)),
    assertz(opponent_pool(2,2)),
    assertz(opponent_pool(2,3)),
    assertz(opponent_pool(2,4)),
    assertz(opponent_pool(2,5)),
    assertz(opponent_pool(2,6)),
    assertz(opponent_pool(3,3)),
    assertz(opponent_pool(3,4)),
    assertz(opponent_pool(3,5)),
    assertz(opponent_pool(3,6)),
    assertz(opponent_pool(4,4)),
    assertz(opponent_pool(4,5)),
    assertz(opponent_pool(4,6)),
    assertz(opponent_pool(5,5)),
    assertz(opponent_pool(5,6)),
    assertz(opponent_pool(6,6)),
    assertz(pieces_own(7)),
    !.

simulated_game:-
    open_hands,
    select_first_move(Left,Right,Turn),
    %write(Left),write(", "),write(Right),write(", "),write(-1),write(", "),
    %write(Turn),nl,
    play(Left,Right,-1,Turn),
    opponent_play(Left,Right,-1,Turn),
    inverse_turn(Turn,Inverse),
    opened_game_logic(Inverse),
    !.

simulate_games(0):-!.
simulate_games(Games):-
    simulated_game,
    reset_game,
    NewGames is Games-1,
    write(NewGames),nl,
    simulate_games(NewGames).









