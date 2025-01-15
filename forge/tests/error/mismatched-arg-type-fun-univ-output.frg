#lang forge/froglet

sig A { }

fun f[a: A] : lone A {
   univ
}

test expect {
    should_error: {some a: A | f[a] = a} is sat
}

