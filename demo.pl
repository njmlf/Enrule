



likes(sam,Food) :-
    indian(Food),
    mild(Food).
likes(sam,Food) :-
    chinese(Food).
likes(sam,Food) :-
    italian(Food).
likes(sam,chips).

indian(curry).
indian(dahl).
indian(tandoori).
indian(kurma).

mild(dahl).
mild(tandoori).
mild(kurma).

chinese(chow_mein).
chinese(chop_suey).
chinese(sweet_and_sour).

italian(pizza).
italian(spaghetti). 

as --> [].
as --> [a], as.

tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
        tree_nodes(Left),
        [Name],
        tree_nodes(Right).
        
move(state(middle,onbox,middle,hasnot),
   grasp,
   state(middle,onbox,middle,has)).
move(state(P,onfloor,P,H),
   climb,
   state(P,onbox,P,H)).
move(state(P1,onfloor,P1,H),
   drag(P1,P2),
   state(P2,onfloor,P2,H)).
move(state(P1,onfloor,B,H),
   walk(P1,P2),
   state(P2,onfloor,B,H)).
canget(state(_,_,_,has)).
canget(State1) :-
   move(State1,_,State2),
   canget(State2).

arc(1,2).
arc(1,3).
arc(2,4).
arc(2,5).
arc(2,6).
arc(5,7).
arc(3,8).
arc(3,9).
arc(9,10).

% path_leaf(N,P) <- P is a path starting at node N, ending
% in a leaf in the graph given by arc/2
path_leaf(Leaf, [Leaf]) :- 
    leaf(Leaf). 

path_leaf(Node1,[Node1|Nodes]):-
    arc(Node1,Node2),
    path_leaf(Node2,Nodes).

leaf(Leaf):- not(arc(Leaf, SomeNode)).


browse :- 
        seeing(Old),      /* save for later */ 
        see(user), 
        write('Enter name of file to browse: '), read(File), 
        see(File),        /* open this file */ 
        repeat, 
        read(Data),       /* read from File */ 
        process(Data),    
        seen,             /* close File */ 
        see(Old),          /*  previous read source */ 
        !.                /* stop now */ 

browse(File) :- 
          seeing(Old),      /* save for later */ 
          see(File),        /* open this file */ 
          repeat, 
          read(Data),       /* read from File */ 
          process(Data),    
          seen,             /* close File */ 
          see(Old),         /*  previous read source */ 
          !.                /* stop now */ 
 
process(end-of-file) :- !. 
process(Data) :-  write(Data), nl, fail. /* 'fail' cause prolog back to 'read(Data)' read data once more */

%%
%% Load a file of Prolog terms into a List.
%%
file_to_list(FILE,LIST) :- 
   see(FILE), 
   inquire([],R), % gather terms from file
   reverse(R,LIST),
   seen.

inquire(IN,OUT):-
   read(Data), 
   (Data == end_of_file ->   % done
      OUT = IN 
        ;    % more
      inquire([Data|IN],OUT) ) . 

time(Goal) :-
    get_time(T0),
    call(Goal),
    get_time(T1),
    !,
    T is T1-T0,
    write(T).

timer(T, Goal) :-
    get_time(T1),
    (T > T1) -> timer(T, Goal);
    get_time(T2),
    call(Goal),
    T3 is T2 - T,
    write(T3).


wait(S, Goal) :-
    S > 0,
    get_time(T0),
    T1 is T0 + S,
    timer(T1, Goal).



parse(L) :- 
    start(S), 
    trans(S,L).

trans(X,[A|B]) :- 
      delta(X,A,Y),   /*  X ---A---> Y */
      write(X),
      write('  '),
      write([A|B]),
      nl,
      trans(Y,B).  
trans(X,[]) :- 
      final(X),
      write(X),
      write('  '),
      write([]), nl.

start(s0).
final(s2).


delta(s0,a,s1).   
delta(s0,b,s0).
delta(s1,a,s1).
delta(s1,b,s2).
delta(s2,a,s2).
delta(s2,b,s2).

path(X, Y, [X, Y]) :- connected(X, Y).
path([X|T], Y, [P|Y]) :- connected(X, Y).

/* 2.15 Graph structures and paths */
edge(1,2).
edge(1,4).
edge(1,3).
edge(2,3).
edge(2,5).
edge(3,4).
edge(3,5).
edge(4,5).

connected(A, B) :- edge(A, B); edge(B, A).

graphpath(A, B, Path) :-
    travel(A, B, [A], P),
    reverse(P, Path).
travel(A, B, Visited, [B|Visited]) :- connected(A, B).
travel(A, B, Visited, Path) :-
    connected(A, X),
    X \== B,
    not(member(X, Visited)),
    travel(X, B, [X|Visited], Path).

/* Search */
/* solve(P) :-
      start(Start),
      search(Start,[Start],Q),
      reverse(Q,P).

search(S,P,P) :- goal(S), !.         % done                  
search(S,Visited,P) :-
     next_state(S,Nxt),              % generate next state   
     safe_state(Nxt),                % check safety          
     no_loop(Nxt,Visited),           % check for loop        
     search(Nxt,[Nxt|Visited],P).    % continue searching... 

no_loop(Nxt,Visited) :-
      \+member(Nxt,Visited). */