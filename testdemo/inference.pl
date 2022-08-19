:- use_module(kb).

ako(Sub, Class) :- fact(Sub, ako, Class).
ako(Sub, Class) :- fact(Sub, ako, SC), ako(SC, Class).

isa(Ins, Class) :- fact(Ins, isa, Class).
isa(Ins, Class) :- fact(Ins, isa, C), ako(C, Class).

rotten(Item) :- fact(Item, rotten, yes), !.
mouldy(Item) :- fact(Item, mouldy, yes), !.
gone_bad(Item) :- rotten(Item), ! ; mouldy(Item).

infested(Item) :-
    isa(Grub, grub), fact(Grub, located_in, Item), !.

edible(Item) :-
    isa(Item, food),
    \+ gone_bad(Item),
    \+ infested(Item), !.
    