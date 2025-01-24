#lang forge/froglet 
option run_sterling off

/*
  This module combines a number of tests that were originally separate files. 
*/

sig University {}
one sig BrownU extends University {}
sig Person {
 father: lone Person,
 mother: lone Person,
 grad: lone University
}
one sig Tim extends Person {}

sig Node {next: Node}

test expect {
  uni0: {father.grad = mother.grad} is forge_error
  uni1: {some father.grad} is forge_error
  uni2some: {some father} is forge_error
  uni2no: {no father} is forge_error
  uni2one: {one father} is forge_error
  uni2lone: {lone father} is forge_error
  uni3: {some father.Tim} is forge_error
  uni4: {some p: Person | reachable[p, father.Tim, father, mother]} is forge_error
  uni5: {some Tim.Tim} is forge_error
  
  reach2: {all n: Node | reachable[next, n, next]} is forge_error "not a singleton"
  reach4: {all n: Node | reachable[n.next]} is forge_error
}



