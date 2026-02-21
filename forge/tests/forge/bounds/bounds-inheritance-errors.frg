#lang forge
option run_sterling off
option verbose 0

-- Separate file due to forge-xrh: breaker state leaks between runs when
-- bounds errors are caught, causing interference in combined test files.

sig Node {edges: set Node}
sig Parent {}
sig Child extends Parent {}

sig StackState {top: lone StackElement}
sig StackElement {}

sig Grandparent {}
sig GParent extends Grandparent {}
sig GChild extends GParent {}

test expect {
  exact_field_atom_not_in_sig: {some top} for {
    StackState = `Foo + `Bar
    StackElement = `A + `B
    top = `Initial -> `A
  } is forge_error "not in bounds for sig"

  field_no_sig_bounds: {some top} for {
    StackElement = `A + `B
    top = `Initial -> `A
  } is forge_error "has no bounds in this"

  inheritance_conflict: {some Child} for {
    Parent = `P1
    Child = `C1
  } is forge_error "child sig Child"

  grandparent_inheritance: {some GChild} for {
    Grandparent = `G1
    GParent = `P1
    GChild = `C1
  } is forge_error "child sig GChild"

  field_ni_atom_not_in_sig: {some Node} for {
    Node = `A + `B
    edges ni `A -> `Z
  } is forge_error

  piecewise_bad_atom: {some Node} for {
    Node = `A + `B
    `A.edges = `Z
  } is forge_error
}
