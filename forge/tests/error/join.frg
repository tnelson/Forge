#lang forge/bsl

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred leftjoin {
    some next.next
}

run {leftjoin}