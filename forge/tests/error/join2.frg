#lang forge/bsl
option run_sterling off
sig Node {
    field: pfunc Node -> Node
}

one sig A extends Node {}

pred leftjoin {
    some A.field
}

run {leftjoin}