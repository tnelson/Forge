#lang forge

sig A { r: set A }

pred p[a:A] { a->a in r }
fun f[a:A]: A { a.r }
fun g[a1:A, a2:A]: A {a1+a2}

run { some a:A | p[a] }