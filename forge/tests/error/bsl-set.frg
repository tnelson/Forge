#lang forge/froglet
option run_sterling off


sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred setcomp {
    some {n : Node | n.next = n}
}

test expect {{setcomp} is sat}