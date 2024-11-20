#lang froglet

sig Person {
    parent: lone Person 
}

pred familyFact {
    all p : Person | { 
        reachable[p, p, parent]
    }
}