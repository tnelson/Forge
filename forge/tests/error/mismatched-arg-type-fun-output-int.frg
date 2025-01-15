#lang forge/froglet

sig A { f1: lone B }
sig B { f2: lone A } 

fun f[a: A] : lone A {
   3
}

test expect {
    should_error: {some a: A | f[a] = a} is sat
}