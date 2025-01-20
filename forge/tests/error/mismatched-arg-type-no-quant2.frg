#lang forge/froglet

sig A { f1: lone B }
sig B { f2: lone A } 
one sig C { f3: lone A }

pred p1[a: A] {
    p2[a.f1, a]
}
pred p2[b1: B, b2: B] {
    some b2.f2 
}

test expect {
    should_error: {p1[C.f3]} is sat
}
