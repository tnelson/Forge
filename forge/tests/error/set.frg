#lang forge/bsl

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred setcomp {
    some {n : Node | n.next = n}
}

run {setcomp}