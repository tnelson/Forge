#lang forge/bsl

sig Node {
    next: one Node
}

one sig A extends Node {}

pred leftjoin {
    some Node.next
}

run {leftjoin}