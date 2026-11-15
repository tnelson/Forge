#lang forge
option run_sterling off
option verbose 0

--  At verbose 0, the unreferenced-predicate warning must not print.
-- (This file is imported from a Racket test file, not run directly. )
sig A {}
pred p { some A }

test suite for p {
  test expect { no_ref: { no A } is sat }
}
