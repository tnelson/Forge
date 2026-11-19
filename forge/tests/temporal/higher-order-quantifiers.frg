#lang forge

option run_sterling off
option verbose 0
option problem_type temporal

/*
  Pardinus doesn't Skolemize past temporal operators at time of writing.
*/

sig A {}
one sig S { var b: set A }

test expect {
  topLevel:        { some X: set A | #X > 1 and always S.b = X } for exactly 3 A is sat
  witnessIsStatic: { some X: set A | #X > 1 and always S.b = X and eventually S.b != S.b' } for exactly 3 A is unsat
  underAlways:     { always (some X: set A | #X > 1 and X = S.b) } for exactly 3 A is forge_error "temporal operator"
}
