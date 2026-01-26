#lang forge
option verbose 0
option run_sterling off

// It should be possible to overwrite options between run commands. Most options
// have no semantic weight, so we can't test them via sat/unsat tests. However,
// some do. One of those is whether integer overflow is allowed.

pred needs_overflow {some i: Int | i > 0 and add[i,1] < 0}

option no_overflow false // default
test expect { overflow_allowed: {needs_overflow} is sat }

option no_overflow true 
test expect { overflow_forbidden: {needs_overflow} is unsat }

