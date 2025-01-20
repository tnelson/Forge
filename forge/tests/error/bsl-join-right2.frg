#lang forge/froglet
option run_sterling off


sig Node {
    next: one Node,
    field: pfunc Node -> Node
}

one sig A, B extends Node {}

pred joinRight {
    some A.field.Node
}

test expect{
     {joinRight} is sat
}