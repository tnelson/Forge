#lang forge/froglet

sig University {}
one sig BrownU extends University {}
sig Person {
 father: lone Person,
 mother: lone Person,
 grad: lone University
}
one sig Tim extends Person {}

pred myPred {
 some Tim.father.grad
 some p: Person | reachable[p, Tim, father, mother]
}

pred p2 {
  no Tim.father
}

test expect {
 myPred is sat
}

example ex1 is { p2 } for {
  no father
}

