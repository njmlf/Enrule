:- begin_tests(inference).  % for plunit

:- include(inference).  % include for everything in local namespace

% add option [nondet] because a choice point is expected
test(ako_fact, [nondet]) :- ako(cox, apple).
test(ako_transitive, [nondet]) :- ako(cox, fruit), ako(cox, food).
% add option [fail] because this should fail
test(ako_fail, [fail]) :- ako(worm, fruit).

test(isa_fact, [nondet]) :- isa(maud, maggot).
test(isa_ako, [nondet]) :- isa(maud, grub).
test(isa_fail, [fail]) :- isa(maud, fruit).

test(rotten) :- rotten(test_subject2).
test(rotten_fail, [fail]) :- rotten(test_subject1).
% No mouldy apple in kb, better add one in [setup()] option
test(mouldy, [setup(assert(fact(a, mouldy, yes)))]) :- mouldy(a).
test(mouldy_fail, [fail]) :- mouldy(test_subject1).

test(gone_bad) :- gone_bad(test_subject2).
test(gone_bad_fail, [fail]) :- gone_bad(test_subject1).

test(infested) :- infested(test_subject3).
test(infested_fail, [fail]) :- infested(test_subject1).

test(edible) :- edible(test_subject1).
test(edible_bad, [fail]) :- edible(test_subject2).
test(edible_maggot, [fail]) :- edible(test_subject3).

:- end_tests(inference).