#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred leftjoin {
    some next.next
}

test expect {{leftjoin} is sat}
