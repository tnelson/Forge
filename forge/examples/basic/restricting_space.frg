#lang forge

option verbose 0
option run_sterling off

/*
  Example of restricting search space in Forge. 
  (The Sudoku example contains a more involved version of this.)

  Partial instances give upper/lower/exact bounds for relations pre-solver. 
  They appear in `example`s, in reusable `inst` helpers, and in the 2nd `for` block of runs, asserts, etc. 
*/

one sig Example {
    f: pfunc Int -> Int
}

// Notice we're asserting unsat as a test that we can't use an integer outside the bounds provided.
assert {some i: Int | i >= 10 and some Example.f[i]}
  is unsat
  for 10 Int // [-512,511]
  for {
    Example = `Example
    // Relational Forge will allow this sort of binding annotation:
    `Example.f in (0+1+2+3+4+5+6+7+8+9) -> (0+1+2+3+4+5+6+7+8+9)
  }

// These partial instances can refer to other partial instances compositionally.
// Note well: bindings are executed in sequence! 
inst optimize_f {
  // By itself, this would be an error because Example isn't bound and `Example isn't defined.
  `Example.f in (0+1+2+3+4+5+6+7+8+9) -> (0+1+2+3+4+5+6+7+8+9)
}

// This will be an error, because we use `Example. Using the sig name would also give an error.
test expect { oops: {} for optimize_f is forge_error }

// Instead, do this. (This example doesn't actually save any lines; the trick is more valuable when your 
// optimizer inst is quite verbose.) Make sure to invoke the big optimizer _after_ binding its dependencies.
assert {some i: Int | i >= 10 and some Example.f[i]}
  is unsat
  for 10 Int // [-512,511]
  for {
    Example = `Example 
    optimize_f
  }

