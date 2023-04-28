#lang forge

sig A {
    i: one Int
}

run {
    some a1: A | a1.i = 3
    --3 < 4
} for exactly 4 A
