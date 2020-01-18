#lang forge

sig A {
    r: set A
}

fun thrice[a: A] : A {
    a.r.r.r
}

pred p {
    #A >= 4
    all a:A { thrice[a] = a }
}

run {p}
