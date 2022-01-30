#lang forge

option verbosity 0

sig Node {
    next: pfunc Node
}
sig Star{
    link: pfunc Node,
    st: pfunc Star
}
one sig A, B extends Node {}
one sig C extends Star{}


pred reach1 {
    // A is reachable from B through next
    reachable[A, B, next]
}

pred reach2 {
    reachable[A, B, next, next]
}

pred reach3 {
    reachable[A, B, next, next, next]
}

pred reach4 {
    reachable[C, A, next]
}

pred reach5 {
    reachable[A, C, st]
}

pred reach6 {
    reachable[A, C, st, link]
}

pred reach7 {
    not (A in B.next)
    reachable[A, B, next]
}

test expect {
    reach1sat: reach1 is sat
    reach2sat: reach2 is sat
    reach3sat: reach3 is sat
    reach4unsat: reach4 is unsat
    reach5unsat: reach5 is unsat
    reach6sat: reach6 is sat
    reach7sat: reach7 is sat
}