#lang froglet

sig University {}
one sig BrownU extends University {}
sig Person {
 father: lone Person,
 mother: lone Person,
 grad: lone University
}
one sig Tim extends Person {}

pred myPred {
 some p: Person | reachable[p, father.Tim, father, mother]
}
