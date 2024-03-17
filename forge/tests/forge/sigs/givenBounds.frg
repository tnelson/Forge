#lang forge

option run_sterling off
 

sig A {}
one sig B {}

test expect {
    noError: {} for exactly 1 A, 2 B is sat
}

