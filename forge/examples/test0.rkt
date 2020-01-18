#lang forge

sig A {
    r: set A
}

sig B extends A {
    s: set B
}

pred p {
    (some B) and ((some A-B) or (some A-B))
    all a:A+A {
        a = a.r
    }
}

/*(define (f a1 a2) (+ a1 a2))*/

fun f[a1: A, a2: A] : A {
    a1+a2
}

run {p}
