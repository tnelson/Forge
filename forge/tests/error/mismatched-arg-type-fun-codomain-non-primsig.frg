#lang forge/froglet

sig A { f1: lone B }
one sig B { f2: lone A }

fun f[a: A] : B {
   a
}

test expect {
    should_error: {some a: A | f[a] = a} is sat
}