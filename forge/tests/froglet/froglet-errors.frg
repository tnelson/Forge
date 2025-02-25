#lang forge/froglet 
option verbose 0
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

sig Node {
  next: lone Node,
  weights: pfunc Node -> Int
}

pred helper[n1,n2: Node] {
  n1 = n2
}

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

  /////////////////////////////////////////////////////////////////////////////
  // We currently trust *Racket*'s arity errors when a model gives the wrong number 
  // of arguments to a helper predicate. 
  // These should not be caught by `is forge_error`; to avoid regressions, we're going to keep 
  // that satisfied only by actual raise-forge-error errors. Instead, see tests/error/main.rkt. 

  // This one isn't ideal, because reachable actually requires _3_, but *Racket* checks for 2+
  //   reach4: {all n: Node | reachable[n.next]} is forge_error "expected: at least 2"
  // If only 2 are given, the reachable *procedure* will throw this error.
  //   reach2plus: {some n1, n2: Node | reachable[n1, n2]} is forge_error "reachable predicate expected at least three arguments"
  // Check that user-defined helper predicates (not just internally defined helpers) give this error, too
  //   helper_arity: {some n: Node | helper[n]} is forge_error "expected: 2"

  // Check order of error production. weights[n2] desugars to n2.weights.
  forgot_start_of_chain_left: {some n1,n2: Node | weights[n2] = 0} is forge_error "Left-hand side of equality was not a singleton atom"
  forgot_start_of_chain_right: {some n1,n2: Node | 0 = weights[n2]} is forge_error "Right-hand side of equality was not a singleton atom"
}



