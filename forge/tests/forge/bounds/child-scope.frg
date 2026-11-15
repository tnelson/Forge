#lang forge
option run_sterling off
option verbose 0

/* The default scope (of 4) applies only to top-level sigs. 
   If a child sig lacks a scope, it's limited by its parent and siblings.
*/

sig A {}
sig B, C extends A {}

test expect childLimitedByParent {
  
  b5_in_a7_sat:  { #B = 5 } for 7 A is sat
  b5_in_a8_sat:  { #B = 5 } for 8 A is sat
  b8_in_a8_sat:  { #B = 8 } for 8 A, 5 Int is sat
  b9_in_a8_unsat: { #B = 9 } for 8 A, 5 Int is unsat

  mixed_sat:     { #B = 2 and #C = 6 } for 8 A, 2 B is sat
  mixed_unsat:   { #B = 2 and #C = 7 } for 8 A, 2 B is unsat
  bare_default_sat:   { #B = 6 } for 6 is sat
  bare_default_unsat: { #B = 7 } for 6 is unsat

  inst_min_sat:  { #B = 7 } for 8 A for { #B >= 6 } is sat
}

abstract sig X {}
sig Y extends X {}
sig Z extends Y {}

test expect grandchild {
  z6_in_x6_sat:   { #Z = 6 } for 6 X is sat
  z7_in_x6_unsat: { #Z = 7 } for 6 X is unsat
}

sig Q {}
one sig Q1, Q2, Q3, Q4 extends Q {}
sig Qn extends Q {}

test expect withOneSiblings {
  // The four `one` sigs use 4 of Q's atoms:
  builtin_default_unsat: { some Qn } is unsat
  qn2_in_6_sat:          { #Qn = 2 } for 6 is sat
  qn3_in_6_unsat:        { #Qn = 3 } for 6 is unsat
}
