#lang forge
option run_sterling off
option verbose 0

/*
  Regression tests. While refactoring the scope->bounds code in November 2026,
  various defects arose in binding expressions involving cardinality.
*/

sig A {}

test expect instCardinality {
  eq_sat:      { #A = 2 } for { #A = 2 } is sat
  eq_unsat:    { #A = 3 } for { #A = 2 } is unsat

  leq_sat:     { #A = 2 } for { #A <= 2 } is sat
  leq_unsat:   { #A = 3 } for { #A <= 2 } is unsat

  lt_sat:      { #A = 1 } for { #A < 2 } is sat
  lt_unsat:    { #A = 2 } for { #A < 2 } is unsat
  lt_zero:     { } for { #A < 0 } is forge_error "cannot be satisfied"

  geq_sat:     { #A = 2 } for { #A >= 2 } is sat
  geq_unsat:   { #A = 1 } for { #A >= 2 } is unsat

  gt_sat:      { #A = 3 } for { #A > 2 } is sat
  gt_unsat:    { #A = 2 } for { #A > 2 } is unsat

  range_sat:   { #A = 2 } for { #A >= 1  #A < 3 } is sat
  range_unsat: { #A = 3 } for { #A >= 1  #A < 3 } is unsat

  // At the default max scope (4).
  geq4_sat:    { #A = 4 } for { #A >= 4 } is sat
  gt3_sat:     { #A = 4 } for { #A > 3 } is sat
  gt3_unsat:   { #A = 3 } for { #A > 3 } is unsat

  // Explicit scopes above the default.
  scoped_gt4_sat:   { #A = 5 } for 6 A for { #A > 4 } is sat
  scoped_gt4_unsat: { #A = 4 } for 6 A for { #A > 4 } is unsat
  scoped_geq6_sat:  { #A = 6 } for 6 A for { #A >= 6 } is sat
  scoped_lt6_unsat: { #A = 6 } for 7 A for { #A < 6 } is unsat

  // Scopes above the default without an explicit max: expect the default to be implicitly raised to fit. 
  eq6_sat:          { #A = 6 } for { #A = 6 } is sat
  gt4_sat:          { #A = 5 } for { #A > 4 } is sat
  geq6_sat:         { #A = 6 } for { #A >= 6 } is sat
  geq6_range_sat:   { #A = 6 } for { #A >= 6  #A <= 8 } is sat
  // The implicit raising is only done minimally, as required by the binding expression.
  ask7_bindge6_sat:          { #A = 7 } for { #A >= 6 } is unsat
 
  // Binding blocks are evaluated in order, so order matters for cardinality here.
  geq_then_leq_7_unsat: { #A = 7 } for { #A >= 6  #A <= 8 } is unsat
  leq_then_geq_7_sat:   { #A = 7 } for { #A <= 8  #A >= 6 } is sat

  // If a scope _is_ declared, it won't be implicitly raised. 
  declared_conflict: { } for 3 A for { #A = 6 } is forge_error "max scope on A was less than numeric min scope"
  inst_conflict:     { } for { #A <= 2  #A >= 3 } is forge_error "max scope on A was less than numeric min scope"
}
