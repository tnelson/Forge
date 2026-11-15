#lang forge
option run_sterling off

-- None of the following should produce a missing-scope warning.
-- (Imported from a Racket test file.)

sig A {}
sig B {}
abstract sig P {}
sig P1, P2 extends P {}
one sig O {}
lone sig L {}

test expect {
  all_scoped:       { some A } for 3 A, 3 B, 2 P1, 2 P2 is sat
  bare_default:     { some A } for 3 is sat
  bare_default_but: { some A } for 3 but 2 A is sat
  no_scopes:        { some A } is sat
  bitwidth_only:    { some A } for 5 Int is sat
  abstract_derived: { some A } for 3 A, 3 B, 2 P1, 2 P2, 5 Int is sat
  inst_bounded:     { some A } for 3 A, 2 P1, 2 P2 for { B = `B0 } is sat
}
