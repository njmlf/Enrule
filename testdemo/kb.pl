:- module(kb, [fact/3]).

fact(fruit, ako, food).
fact(apple, ako, fruit).
fact(cox, ako, apple).
fact(braeburn, ako, apple).
fact(bramley, ako, apple).
fact(granny_smith, ako, apple).

fact(test_subject1, isa, bramley).
fact(test_subject1, rotten, no).
fact(test_subject1, mouldy, no).

fact(test_subject2, isa, bramley).
fact(test_subject2, rotten, yes).
fact(test_subject2, mouldy, no).

fact(test_subject3, isa, granny_smith).
fact(test_subject3, rotten, no).
fact(test_subject3, mouldy, no).
fact(maggot, ako, grub).
fact(maud, isa, maggot).
fact(maud, located_in, test_subject3).
