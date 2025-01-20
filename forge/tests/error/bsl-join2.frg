#lang forge/froglet
option run_sterling off

sig Node {
    field: pfunc Node -> Node
}

one sig A extends Node {}

pred leftjoin {
    some A.field
}

test expect {{leftjoin} is sat}