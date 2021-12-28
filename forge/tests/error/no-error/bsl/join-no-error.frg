#lang forge/bsl

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred leftjoin {
    some A.next
    some A.next.next.next
}

run {leftjoin}