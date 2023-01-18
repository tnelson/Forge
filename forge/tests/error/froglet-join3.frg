#lang froglet
option run_sterling off

sig Node {
    next: one Node
}

one sig A extends Node {}

pred leftjoin {
    some Node.next
}

test expect {{leftjoin} is sat}
