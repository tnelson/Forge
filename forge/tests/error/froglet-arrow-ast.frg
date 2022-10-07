#lang froglet

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred arrow {
    A->next in next
}

test expect {
    {arrow} is sat
}
