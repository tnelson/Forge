#lang forge

sig A { r: set A }

pred p[a:A] { a->a in r }

run { some a:A | p[a] }