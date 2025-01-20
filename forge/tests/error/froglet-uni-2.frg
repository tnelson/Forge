#lang forge/froglet
option run_sterling off

sig University {}
one sig BrownU extends University {}
sig Person {
 father: lone Person,
 mother: lone Person,
 grad: lone University
}
one sig Tim extends Person {}

pred myPred {
  some father
}
