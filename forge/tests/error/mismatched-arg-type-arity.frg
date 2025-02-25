#lang forge/froglet
option verbose 0

sig A { f1: lone B }
sig B { f2: lone A } 
pred p1[a: A] {
    p2[a.f1, a]
}
pred p2[b1: B, b2: B] {
    some b2.f2 
}

test expect {
    should_error: {some x: A | p1[x]} is sat
}
