#lang forge
option run_sterling off
option verbose 0

/*
  Bounds declaration error handling tests.
  Regression tests for fixes in dbb3dd1a, 250a9880, 7dadf90b, fb8c114c.

  Covers: sig bounds, field bounds, piecewise bounds, inheritance,
  multiplicity, and various error conditions.
*/

sig Node {edges: set Node}
sig Parent {}
sig Child extends Parent {}

/////////////////////////////////////////////////////////////////////////////
// Valid combinations - these should all be sat
/////////////////////////////////////////////////////////////////////////////

test expect validSigBounds {
  sig_eq_only: {some Node} for {Node = `A + `B} is sat
  sig_in_only: {some Node} for {Node in `A + `B + `C} is sat
  sig_eq_in: {some Node} for {Node = `A + `B  Node in `A + `B + `C} is sat
  sig_eq_ni: {some Node} for {Node = `A + `B  Node ni `A} is sat
  sig_in_eq: {some Node} for {Node in `A + `B + `C  Node = `A + `B} is sat
  sig_ni_eq: {some Node} for {Node ni `A  Node = `A + `B} is sat
  sig_eq_eq_same: {some Node} for {Node = `A + `B  Node = `A + `B} is sat
  sig_in_in_same: {some Node} for {Node in `A + `B  Node in `A + `B} is sat
  sig_in_in_diff: {some Node} for {Node in `A + `B + `C  Node in `B + `C + `D} is sat
  sig_in_ni_valid: {some Node} for {Node in `A + `B + `C  Node ni `A + `B} is sat
}

test expect validFieldBounds {
  field_in_ni_valid: {some Node} for {
    Node = `A + `B + `C
    edges in `A -> `B + `A -> `C + `B -> `C
    edges ni `A -> `B
  } is sat
}

test expect validPiecewise {
  piecewise_field: {some Node} for {
    Node = `A + `B + `C
    `A.edges = `B
    `B.edges = `C
  } is sat

  piecewise_in: {some Node} for {
    Node = `A + `B + `C
    `A.edges in `B + `C
  } is sat

  piecewise_ni: {some Node} for {
    Node = `A + `B + `C
    `A.edges ni `B
  } is sat
}

test expect validInheritance {
  inheritance_valid: {some Child} for {
    Parent = `P1 + `C1
    Child = `C1
  } is sat
}

/////////////////////////////////////////////////////////////////////////////
// ni-only bounds (no in/= declaration)
// Upper bound derived from numeric scope, lower from ni declaration.
/////////////////////////////////////////////////////////////////////////////

test expect niOnly {
  sig_ni_only: {some Node} for {Node ni `A + `B} is sat
  sig_ni_ni_same: {some Node} for {Node ni `A + `B  Node ni `A + `B} is sat
  sig_ni_ni_diff: {some Node} for {Node ni `A + `B  Node ni `C + `D} is sat
}

/////////////////////////////////////////////////////////////////////////////
// Field bounds referencing atoms not in the owning sig
/////////////////////////////////////////////////////////////////////////////

test expect fieldInValidation {
  field_in_atom_not_in_sig: {some Node} for {
    Node = `A + `B
    edges in `A -> `Z
  } is forge_error "not in bounds for sig"
}

/////////////////////////////////////////////////////////////////////////////
// Conflicting bound declarations (in/ni/= combinations that contradict)
/////////////////////////////////////////////////////////////////////////////

test expect boundConflicts {
  in_ni_conflict_sig: {some Node} for {
    Node in `A + `B
    Node ni `C
  } is forge_error "lower bound includes `C"

  in_ni_conflict_field: {some Node} for {
    Node = `A + `B + `C
    edges in `A -> `B
    edges ni `A -> `C
  } is forge_error "lower bound includes `A -> `C"

  sig_eq_eq_diff: {some Node} for {
    Node = `A + `B
    Node = `C + `D
  } is forge_error "Bound conflict for Node"

  sig_ni_in_conflict: {some Node} for {
    Node ni `C
    Node in `A + `B
  } is forge_error "lower bound includes `C"
}

/////////////////////////////////////////////////////////////////////////////
// Inheritance bounds: field and sig atoms checked against hierarchy
// See bounds-inheritance-errors.frg for additional inheritance tests.
/////////////////////////////////////////////////////////////////////////////

sig StackState {top: lone StackElement}
sig StackElement {}

sig Grandparent {}
sig GParent extends Grandparent {}
sig GChild extends GParent {}

/////////////////////////////////////////////////////////////////////////////
// Multiplicity violations caught as unsat (not as forge_error)
// The solver rejects these but doesn't explain why. Pre-validation could
// produce better messages; for now, test that they are at least unsat.
/////////////////////////////////////////////////////////////////////////////

one sig OneNode {}
lone sig LoneNode {}
abstract sig Animal {}
sig Dog extends Animal {}
sig Cat extends Animal {}
sig Sibling1 extends Parent {}
sig Sibling2 extends Parent {}
sig LinkedNode {next: one Node}

test expect multiplicityViolations {
  one_sig_multiple: {some OneNode} for {
    OneNode = `A + `B
  } is unsat

  lone_sig_multiple: {some LoneNode} for {
    LoneNode = `A + `B
  } is unsat

  abstract_uncovered_atom: {some Animal} for {
    Animal = `A + `B + `C
    Dog = `A
    Cat = `B
  } is unsat

  sibling_overlap: {some Parent} for {
    Parent = `A + `B
    Sibling1 = `A
    Sibling2 = `A
  } is unsat

  field_one_too_many: {some Node} for {
    Node = `A + `B
    LinkedNode = `A + `B
    next = `A -> `B + `A -> `A
  } is unsat

  field_one_missing: {some Node} for {
    Node = `A + `B
    LinkedNode = `A + `B
    next = `A -> `B
  } is unsat
}

/////////////////////////////////////////////////////////////////////////////
// Piecewise bound errors
/////////////////////////////////////////////////////////////////////////////

test expect piecewiseErrors {
  piecewise_conflict: {some Node} for {
    Node = `A + `B + `C
    edges = `A -> `B
    `A.edges = `C
  } is forge_error "may not be combined"

  piecewise_bad_owner: {some Node} for {
    Node = `A + `B
    `Z.edges = `A
  } is forge_error "was bounded for atom"
}
