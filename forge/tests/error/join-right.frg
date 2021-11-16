#lang forge/bsl

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred leftjoin {
    some next.B
}

run {join-right}