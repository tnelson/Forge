#lang forge

sig A {
    r: set A
}

fun thrice[a: A] : A {
    a.r.r.r
}

pred p {
    some A
    all a:A { thrice[a] = a }
}

run {p} for 3 A
