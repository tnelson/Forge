#lang froglet

sig Person {
    age : one Int
}

pred is_adult {
    all p : Person | p.age >= 18
}