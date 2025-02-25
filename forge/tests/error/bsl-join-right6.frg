#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
    next: one Node,
    field: pfunc Node -> Node
}

one sig A, B extends Node {}

pred joinRight {
    some A.(field.A)
}

test expect {{joinRight} is sat}