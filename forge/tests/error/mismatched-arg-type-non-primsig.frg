#lang forge/froglet

sig A {
    f2: lone B
}
one sig B extends A {}
one sig C extends A {}

pred p1[a: A] {
    p2[a]
}
pred p2[b: B] {
    some b.f2 
}

test expect {
    should_error: {some x: A | p1[x]} is sat
}
