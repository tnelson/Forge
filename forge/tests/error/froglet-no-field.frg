#lang froglet

// no father = disallowed at toplevel

sig Person {
 father: lone Person
}
one sig Tim extends Person {}

pred myPred {
  no father
}

