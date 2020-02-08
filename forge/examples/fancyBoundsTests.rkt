#lang forge


sig A { r: set A }
sig B { s: set B }

run {
    some r
} for {
    3 < #A < 4
    r in cotree
    B = B0+B1+B2
    s = B->(B0+B1)
}