#lang forge/bsl

sig A {
    f2: lone B
}
one sig B extends A {}
one sig C extends A {}

pred p[c: C] {
    some c.f2
}

test expect {
    should_error: {some x: B | p[x]} is sat
}
