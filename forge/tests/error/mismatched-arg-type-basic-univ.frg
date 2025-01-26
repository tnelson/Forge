#lang forge
option verbose 0

sig A { f1: lone B }
sig B { f2: lone A } 
pred p1[a: univ] {
    p2[a]
}
pred p2[b: B] {
    some b.f2 
}

test expect {
    should_error: {some x: univ | p1[x]} is sat
}
