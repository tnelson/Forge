#lang forge 

sig A {}
one sig B {}

test expect {
    noError: {} for exactly 1 A, 2 B is sat
}

