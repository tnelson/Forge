#lang forge
option run_sterling off

-- A test in a `test suite for p` that never mentions p produces a warning.

sig A {}
pred p { some A }

test suite for p {
  test expect { no_ref: { no A } is sat }
}
