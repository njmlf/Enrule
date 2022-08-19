:- begin_tests(lists).
:- use_module(library(lists)).

test(reverse) :-
        reverse([a,b], [b,a]).

test(member) :-
        member(3, [1,2,3]).

test(a) :-
        A is 2^3,
        assertion(integer(A)),
        assertion(A == 8).

:- end_tests(lists).
