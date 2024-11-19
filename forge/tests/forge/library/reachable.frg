#lang forge

option run_sterling off
option verbosity 0

sig Node {
    next: pfunc Node
}
sig Star{
    link: pfunc Node,
    st: pfunc Star
}
one sig A, B extends Node {}
one sig C extends Star {}

test expect {
    reach1sat: {reachable[A, B, next]} is sat
    reach2sat: {reachable[A, B, next, next]} is sat
    reach3sat: {reachable[A, B, next, next, next]} is sat
    reach4unsat: {reachable[C, A, next]} is unsat
    reach5unsat: {reachable[A, C, st]} is unsat
    reach6sat: {reachable[A, C, st, link]} is sat
    reach7sat: {
        not (A in B.next)
        reachable[A, B, next]
    } is sat
}