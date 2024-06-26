#lang froglet

sig Person {
    parent : lone Person
}

one sig A extends Person {}
one sig B extends Person {}

pred facts {
    A.parent = B
}

