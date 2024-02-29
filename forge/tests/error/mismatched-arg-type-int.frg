#lang forge/bsl

sig A { f1: lone B }
sig B { f2: lone A } 
pred p1[a: A] {
    p2[a]
}
pred p2[b: Int] {
    b > 0
}

test expect {
    should_error: {some x: A | p1[x]} is sat
}
