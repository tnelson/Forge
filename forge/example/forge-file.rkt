#lang forge

abstract sig Person {}

sig A extends Person {
    friend: set B
}
sig B extends Person {}

one sig AKing extends A {}
one sig BKing extends B {}

pred AreFriends[a: A, b: B] {
    a->b in friend
}

fun getFriends[a: A]: set B {
    a.friend
}

everyoneHasFriends : run { 
    all a: A | some getFriends[a]
    all b: B | some friend.b
    AreFriends[AKing, BKing]
}