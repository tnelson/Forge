#lang forge/bsl

sig Node {
    field: pfunc Node -> Node
}

one sig A extends Node {}

pred leftjoin {
    some A.field
}

run {leftjoin}