#lang forge
// Test that the FINAL option value from an imported module is inherited
// helpers/import-options-B.frg sets no_overflow to false, then true
// We should inherit no_overflow = true (the final value)
open "helpers/import-options-B.frg"

// This pred requires integer overflow to be SAT
// With no_overflow = true, overflow is forbidden, so this should be UNSAT
pred needs_overflow { some i: Int | i > 0 and add[i,1] < 0 }
test expect { overflow_blocked: {needs_overflow} is unsat }
