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
  uni5: {some Tim.Tim} is forge_error "is not a field"
  
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

  /////////////////////////////////////////////////////////////////////////////
  // Relational operators - all should be blocked in Froglet with specific messages
  /////////////////////////////////////////////////////////////////////////////

  op_union: {some n: Node | n in n + n.next} is forge_error "operator is not used in Froglet"
  op_difference: {some n: Node | n in Node - n} is forge_error "operator is not used in Froglet"
  op_intersection: {some n: Node | n in Node & n} is forge_error "operator is not used in Froglet"
  op_product: {some n: Node | some n -> n} is forge_error "only used for field declaration"
  op_transitive_closure: {some n: Node | n in ^next} is forge_error "operator is not used in Froglet"
  op_reflexive_closure: {some n: Node | n in *next} is forge_error "operator is not used in Froglet"
  op_transpose: {some n: Node | n in ~next} is forge_error "operator is not used in Froglet"

  /////////////////////////////////////////////////////////////////////////////
  // Relational constants
  // DECISION: none and univ are intentionally ALLOWED in Froglet
  //   - none: useful as empty set
  //   - univ: useful as "top" type
  //   - iden: uncertain - identity relation has no clear OOP equivalent
  /////////////////////////////////////////////////////////////////////////////

  const_none_allowed: {no none} is sat
  // const_univ_allowed: {some univ} is sat  // crashes: "not a singleton value: univ"
  // const_iden: {some iden} is forge_error "not used in Froglet"  // decision pending

  /////////////////////////////////////////////////////////////////////////////
  // Box join syntax - should have same restrictions as dot join
  // FINDING: box_join_sig_as_field gets generic "arity 0" error, not Froglet-friendly
  /////////////////////////////////////////////////////////////////////////////

  box_join_bare_relation: {some n: Node | next[Node] = n} is forge_error "not a singleton"
  box_join_sig_as_field: {some p: Person | Tim[p] = p} is forge_error "is not a field"

  /////////////////////////////////////////////////////////////////////////////
  // Bare relation equality (comparing relations, not singletons)
  /////////////////////////////////////////////////////////////////////////////

  bare_relation_eq: {father = mother} is forge_error "did not result in a singleton"

  /////////////////////////////////////////////////////////////////////////////
  // Temporal operators - blocked in Froglet (no temporal semantics)
  // FINDING: These fail at parse time with "use of LTL operator without temporal
  // problem_type declared" - this is correct behavior, just not a forge_error
  /////////////////////////////////////////////////////////////////////////////

  // temporal_always: {always some Node} is forge_error "not available in Froglet"
  // temporal_eventually: {eventually some Node} is forge_error "not available in Froglet"
  // etc. - all temporal operators blocked at parse time

  /////////////////////////////////////////////////////////////////////////////
  // Atom literals in formulas - should only be allowed in inst blocks
  // Known gap: atom literals in formulas are not yet rejected outside inst blocks
  /////////////////////////////////////////////////////////////////////////////

  // atom_in_formula: {some p: Person | p = `Person0} is forge_error "not allowed"

  /////////////////////////////////////////////////////////////////////////////
  // Quantifying over relations (non-arity-1 domains)
  /////////////////////////////////////////////////////////////////////////////

  quantify_over_relation: {all r: father | some r} is forge_error "arity"

  /////////////////////////////////////////////////////////////////////////////
  // Override operator (++) - relational concept
  // FINDING: Gets generic arity error, not Froglet-specific message
  /////////////////////////////////////////////////////////////////////////////

  // op_override: {some n: Node | n in next ++ next} is forge_error "not used in Froglet"

  /////////////////////////////////////////////////////////////////////////////
  // Prime operator (') - temporal concept
  // FINDING: Blocked at parse time with "use of LTL operator without temporal
  // problem_type declared" - correct behavior, just not a forge_error
  /////////////////////////////////////////////////////////////////////////////

  // op_prime: {some n: Node | some n.next'} is forge_error "not used in Froglet"

  /////////////////////////////////////////////////////////////////////////////
  // Join chains - should work (positive test)
  /////////////////////////////////////////////////////////////////////////////

  join_chain_valid: {some p: Person | some p.father.mother.grad} is sat
}



