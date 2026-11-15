#lang forge
option run_sterling off
option verbose 0

/* If a sig has no explicit scope, `for N` will replace its scope. */

sig A {}
sig B {}
abstract sig P {}
sig P1, P2 extends P {}

pred fiveAs { #A = 5 }

test expect defaultScope {
  raised_sat:         { #A = 5 } for 5 is sat
  raised_unsat:       { #A = 6 } for 5 is unsat
  lowered_sat:        { #A = 2 } for 2 is sat
  lowered_unsat:      { #A = 3 } for 2 is unsat
  zero_unsat:         { some A } for 0 is unsat
  but_sat:            { #A = 5 and #B = 2 } for 5 but 2 B is sat
  but_unsat:          { #B = 3 } for 5 but 2 B is unsat
  ints_unaffected:    { some i: Int | i = 7 } for 2 is sat
  child_sat:          { #P1 = 5 } for 5 is sat
  child_unsat:        { #P1 = 6 } for 5 is unsat
  

  inst_card_raised:   { #A = 6 } for 5 for { #A >= 6 } is sat
}

assert { #A = 5 } is sat for 5
assert { #A = 5 } is consistent with fiveAs for 5
