#lang forge/temporal
option run_sterling off
option verbose 0
option max_tracelength 3

/*
  In Temporal Forge, `is linear` constrains every state, whichever encoding is used.
  We do this for consistency between the "this can be done with bounds alone" and 
  "some other breaker interferes, this needs a formula" cases.
*/

sig S { var next: lone S, g: set S }
-- Because field g's bound names S atoms, the linear breaker needs a formula.
inst namedInField { S = `S0 + `S1 + `S2  g = `S0 -> `S1  next is linear }

test expect {
  linearVarFrozen: { next' != next } for exactly 3 S for { next is linear } is unsat
  linearVarFrozenOnFallback: { next' != next } for namedInField is unsat
}
