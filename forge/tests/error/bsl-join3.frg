#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
    next: one Node
}

one sig A extends Node {}

pred leftjoin {
    some Node.next
}

test expect {{leftjoin} is sat}
