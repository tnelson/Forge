#lang forge/froglet

sig A { f1: lone B }
sig B { f2: lone A } 

fun f[b: B, a: A] : lone B {
    a.f1
}

test expect {
    should_error: {some b1, b2: B | f[b1, b2] = b2} is sat
}