#lang forge
option run_sterling off
option verbose 0

-- Regression test for `pred wheat` crash bug.
sig A {}
pred wheat someA { some A }

test expect {
  wheatSat: { someA } is sat
  wheatUnsat: { someA and no A } is unsat
}
