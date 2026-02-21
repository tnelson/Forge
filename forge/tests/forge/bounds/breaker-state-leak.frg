#lang forge
option run_sterling off
option verbose 0

-- Regression test for forge-xrh: breaker state leaks between runs
-- when a bounds error is caught via `is forge_error`.
--
-- The bug: send-to-solver.rkt calls break-rel (populating breaker state)
-- BEFORE get-bounds. If get-bounds raises a bounds error, clear-breaker-state
-- at line 192 is never reached. The next run inherits stale sbounds.
--
-- This file puts a bounds-error run first, then a clean run second.
-- If the bug is present, the second run fails or gives wrong results.

sig Parent {}
sig Child extends Parent {}

-- Run 1: triggers a bounds error (child not subset of parent upper bound).
-- This error should be caught, but breaker state must be cleaned up.
test expect {
  bad_bounds: {some Child} for {
    Parent = `P1
    Child = `C1
  } is forge_error "child sig Child"
}

-- Run 2: a completely clean, valid run using the same sigs.
-- If breaker state leaked from Run 1, this may fail spuriously.
test expect {
  clean_after_error: {some Child} for {
    Parent = `P1 + `C1
    Child = `C1
  } is sat
}
