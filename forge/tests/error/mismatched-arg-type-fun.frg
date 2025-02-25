#lang forge/froglet
option verbose 0

sig A { f1: lone B }
sig B { f2: lone A } 

fun f[a: A] : lone B {
    a.f1
}

test expect {
    should_error: {some b: B | f[b] = b} is sat
}