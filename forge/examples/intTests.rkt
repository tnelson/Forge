#lang forge

sig A {
    i: one Int
}

run {
    --some a1: A | some a2: A | a1.i > a2.i
    3 < 4
} for exactly 4 A
