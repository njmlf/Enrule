factorial(0, 1).                                /* 阶乘 */
factorial(X, R) :-
    X1 is X-1,
    factorial(X1, R1),
    R is R1 * X.

current(S) :-
    get_time(S).

lookup(X, [X|_], [V|_], V).
lookup(X, [_|A], [_|V], Y) :-
    lookup(X, A, V, Y).

hanoi(1,X,Y,_) :-  
    write('Move top disk from '), 
    write(X), 
    write(' to '), 
    write(Y), 
    nl. 
hanoi(N,X,Y,Z) :- 
    N>1, 
    M is N-1, 
    hanoi(M,X,Z,Y), 
    hanoi(1,X,Y,_), 
    hanoi(M,Z,Y,X).

parent(a, b).
parent(a, c).
parent(b, e).
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \== Y.

node_depth(a, 0) :- 
    !.

node_depth(Node, D) :- 
    parent(AnotherNode, Node),
    node_depth(AnotherNode, D1),
    D is D1+1.


% is_leaf(e).
is_leaf(Node) :-
    not(parent(Node, -)).

path(a).
path(Node) :-
    parent(AnotherNode, Node),
    path(AnotherNode),
    write(AnotherNode),
    write(' --> '). 
location(Node) :-
    path(Node),
    write(Node),
    nl.

push(X, Y, [X|Y]).
pop([]) :- !, fail.
pop([P|L], L, P).

move(a, action1, b).
move(b, action2, e).
move(From, [Steps, Action], To) :-
    move(From, Steps, D),
    move(D, Action, To).

takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

:- op(1000, xfy, 'and').
:- op(1000, xfy, 'or').
:- op(1000, fy, 'not').

find_vars(Number, V, V) :-
    member(Number, [1, 0]), !.
find_vars(X, Vin, Vout) :-
    atom(X),
    (member(X, Vin) -> Vout = Vin;
        Vout = [X|Vin]).
find_vars(X and Y, Vin, Vout) :-
    find_vars(X, Vin, Vout1),
    find_vars(Y, Vout1, Vout).
find_vars(X or Y, Vin, Vout) :-
    find_vars(X, Vin, Vout1),
    find_vars(Y, Vout1, Vout).
find_vars(not X, Vin, Vout) :-
    find_vars(X, Vin, Vout).

bool_and(0,0,0).            
bool_and(0,1,0).            
bool_and(1,0,0).      
bool_and(1,1,1).
bool_or(0,0,0).
bool_or(0,1,1).
bool_or(1,0,1).
bool_or(1,1,1).  
bool_not(0,1).
bool_not(1,0).    

bool_exp(X, Vars, Vals, V) :-
    atom(X),
    lookup(X, Vars, Vals, V).
bool_exp(X and Y, Vars, Vals, V) :-
    bool_exp(X, Vars, Vals, Vx),
    bool_exp(Y, Vars, Vals, Vy),
    bool_and(Vx, Vy, V).
bool_exp(X or Y, Vars, Vals, V) :-
    bool_exp(X, Vars, Vals, Vx),
    bool_exp(Y, Vars, Vals, Vy),
    bool_or(Vx, Vy, V).
bool_exp(not X, Vars, Vals, V) :-
    bool_exp(X, Vars, Vals, Vx),
    bool_not(Vx, V).

next([0|R],[1|R]).
next([1|R],[0|S]) :- next(R,S).

add_bits_one(X, Y) :-
    reverse(X, N),
    next(N, M),
    reverse(M, Y).

initial_assign([], []).
initial_assign([_|T], [0|V]) :-
    initial_assign(T, V).

write_row(E,Vars,A) :- 
    write('  '), write(A), write('        '), 
    bool_exp(E, Vars, A, Vexp), write(Vexp), nl,
    (add_bits_one(A, A1) -> write_row(E, Vars, A1); true).

tt(E) :-
    find_vars(E, [], Vout),
    reverse(Vout, Vars),
    initial_assign(Vars, InitVals),
    write('  '), write(Vars), write('    '), write(E), nl,
    write('-----------------------------------------'), nl,
    write_row(E, Vars, InitVals),
    write('-----------------------------------------'), nl.

from_to(F, F, L, [F|L]).
from_to(F, T, A, L) :-
    T > F,
    N is T - 1,
    from_to(F, N, [T|A], L).
from_to(F, T, L) :- from_to(F, T, [], L).

sum([], S, S).
sum([H|T], Acc, S) :-
    NewAcc is Acc + H, !,
    sum(T, NewAcc, S).
sum(L, S) :-
    from_to(1, L, List),
    sum(List, 0, S).

reverse([], R, R).
reverse([H|T], A, R) :-
     reverse(T, [H|A], R).
new_reverse(L, R) :- 
    reverse(L, [], R).
   
min([H|T], M) :- min(T, H, M).
min([], M, M).
min([H|T], A, M) :- 
    H < A, !,
    min(T, H, M).
min([H|T], A, M) :- 
    H >= A,
    min(T, A, M).

:- listing(X).
:- write('That\'s all ..............................'),nl.