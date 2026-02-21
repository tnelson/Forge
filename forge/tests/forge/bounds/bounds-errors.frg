#lang forge
option run_sterling off
option verbose 0

/*
  Bounds declaration error handling tests.
  Consolidation of bounds-error-investigation/ test cases.

  Covers: sig bounds, field bounds, piecewise bounds, inheritance,
  multiplicity, and various error conditions.

  Categories:
    1. Valid combinations (should pass)
    2. Crash bugs (ni-only → contract violation)
    3. Silent bugs (invalid atoms accepted)
    4. Bad error messages (internal representations leaked)
    5. Generic error messages (solver unsat, no explanation)
    6. Good error messages (already well-handled)
    7. Parse-level errors (syntax limitations)
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
// BUG: ni-only bounds → contract violation (breaks.rkt:114)
// Upper bound is #f when unspecified, but code expects a set.
// Error: "set->list: contract violation: expected: set? given: #f"
/////////////////////////////////////////////////////////////////////////////

-- ni-only bounds: upper derived from scope, lower from ni declaration.
test expect niOnly {
  sig_ni_only: {some Node} for {Node ni `A + `B} is sat
  sig_ni_ni_same: {some Node} for {Node ni `A + `B  Node ni `A + `B} is sat
  sig_ni_ni_diff: {some Node} for {Node ni `A + `B  Node ni `C + `D} is sat
}

/////////////////////////////////////////////////////////////////////////////
// BUG: Silent acceptance of invalid atoms
// Field 'in' bounds accept atoms not in sig without error.
// Field 'ni' bounds correctly error for the same case → inconsistent.
/////////////////////////////////////////////////////////////////////////////

-- Uncomment after fixing: should be forge_error but currently passes silently.
-- test expect silentBugs {
--   field_in_atom_not_in_sig: {some Node} for {
--     Node = `A + `B
--     edges in `A -> `Z
--   } is forge_error
-- }

/////////////////////////////////////////////////////////////////////////////
// Bad error messages: sigs-functional.rkt:1023
// Shows internal #<set: (A) (B)> instead of friendly `A, `B, `C format
/////////////////////////////////////////////////////////////////////////////

test expect badMessagesSigsFunctional {
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
// Bad error messages: send-to-solver.rkt
// Shows raw tuples like ((C1) (P1)) instead of friendly format
/////////////////////////////////////////////////////////////////////////////

sig StackState {top: lone StackElement}
sig StackElement {}

-- These tests work in isolation but fail in this combined file due to
-- stale breaker state leaking between tests. See bounds-inheritance-errors.frg.

sig Grandparent {}
sig GParent extends Grandparent {}
sig GChild extends GParent {}

/////////////////////////////////////////////////////////////////////////////
// Generic "Invalid Example" messages
// Solver returns unsat but message doesn't explain WHY.
// These are correctly caught as errors, but the messages could be better.
/////////////////////////////////////////////////////////////////////////////

one sig OneNode {}
lone sig LoneNode {}
abstract sig Animal {}
sig Dog extends Animal {}
sig Cat extends Animal {}
sig Sibling1 extends Parent {}
sig Sibling2 extends Parent {}
sig LinkedNode {next: one Node}

-- These don't produce forge_error — the solver returns unsat or "invalid example"
-- without explaining WHY. Pre-validation could catch these with better messages.
-- For now, test that they at least aren't sat.

test expect genericMessages {
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
// Good error messages (already well-handled, kept as regression tests)
/////////////////////////////////////////////////////////////////////////////

-- BUG: Even the "good" error messages crash the test file.
-- Bounds-processing errors are not raised via raise-forge-error,
-- so is forge_error can't catch them. ALL bounds errors need to be
-- converted to use raise-forge-error.

-- test expect goodMessages {
--   piecewise_conflict: {some Node} for {
--     Node = `A + `B + `C
--     edges = `A -> `B
--     `A.edges = `C
--   } is forge_error "may not be combined"
--
--   piecewise_bad_owner: {some Node} for {
--     Node = `A + `B
--     `Z.edges = `A
--   } is forge_error "was bounded for atom"
-- }
