#lang forge
option run_sterling off
option verbose 0

/*
  A `one` sig bound to be subset of a set of atoms will still only have one 
  atom in any instance.
*/

one sig A {}
sig Q { f: lone A }
sig P {}
one sig B extends P {}

test expect oneSigInBind {
  two_A_unsat:  { #A = 2 } for { A in `X + `Y } is unsat
  one_A_sat:    { #A = 1 } for { A in `X + `Y } is sat
  
  a_can_be_x_sat: { some f } for { A in `X + `Y  Q = `Q0  f in `Q0 -> `X } is sat
  a_can_be_y_sat: { some f } for { A in `X + `Y  Q = `Q0  f in `Q0 -> `Y } is sat
  eq_sat:         { #A = 1 } for { A = `X } is sat
  ni_sat:         { #A = 1 } for { A ni `X } is sat
  
  child_two_unsat: { #B = 2 } for { P = `X + `Y  B in `X + `Y } is unsat
  child_one_sat:   { #B = 1 } for { P = `X + `Y  B in `X + `Y } is sat
}
