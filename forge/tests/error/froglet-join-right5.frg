#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
    next: lone Node,
    field: pfunc Node -> Node
}

one sig A extends Node {}

pred rightjoin {
    reachable[A, A.field.Node, next]
}

test expect{ {rightjoin} is sat}
