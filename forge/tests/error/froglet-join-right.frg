#lang froglet
option run_sterling off
option verbose 0

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred joinRight {
    some next.B
}

test expect{
     {joinRight} is sat
}
