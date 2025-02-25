#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred setSingle {
    A.next = Node
}

test expect {{setSingle} is sat}
