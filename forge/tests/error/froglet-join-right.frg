#lang froglet

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
