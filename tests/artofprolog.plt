:- begin_tests(artofprolog).

test("integer type") :-
        integer(2).
test("float type") :-
        float(2.34).
test("number type") :-
        number(2.34).
test("atom type") :-
        atom(a).
test("atomic type - atom") :-
        atomic(a).
test("atomic type - number") :-
        atomic(3).
test("compound type") :-
        compound(a(x,y)).
test("functor type") :-
        functor(father(haran,lot), Term, Arity),
        Term == father,
        Arity == 2.
:- end_tests(artofprolog).
