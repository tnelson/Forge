#lang froglet

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

sig State {
    torch: one Position,
    spent: one Int
}

pred ValidState[s: State] {
    s.torch = 0
}

