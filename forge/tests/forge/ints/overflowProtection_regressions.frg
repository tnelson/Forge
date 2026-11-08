#lang forge
option verbose 0
option run_sterling off

/*
   Regression tests for Alloy issue # 330, which impacts any tool built atop 
   Pardinus. The issue was a soundness problem: with overflow-prevention on,
   unsatisfiable queries involving implication could produce spurious instances.
*/ 

sig Node {}
option no_overflow true

test expect {
  -- Minimal example from the filed issue. 
  impl_noOverflow_4: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) => false
  } for 4 Int is unsat
  -- Check alternative bitwidth.
  impl_noOverflow_3: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) => false
  } for 3 Int is unsat

  -- Check for an IFF version. This shouldn't be needed (since IFF desugars) but 
  -- having the test protects from regressions. 
  iff_noOverflow_4: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) <=> false
  } for 4 Int is unsat

  -- Formala-level if-then-else, with tautology on LHS.
  ite_formula_noOverflow_4: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) => false else false
  } for 4 Int is unsat

  -- Expression-level if-then-else, with tautology on LHS.
  ite_expr_noOverflow_4: {
    some x: Int | some ((add[x,1] < 0 or add[x,1] >= 0) => none else x)
  } for 4 Int is unsat

  -- A control: the original issue didn't manifest when the implication was rewritten as disjunction.
  impl_neg_noOverflow_4: {
    some x: Int | not ((add[x,1] < 0) or (add[x,1] >= 0)) or false
  } for 4 Int is unsat

  -- Tests for non-vacuity under overflow prevention
  legit_impl_noOverflow_4: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) => (x = 3)
  } for 4 Int is sat
  legit_iff_noOverflow_4: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) <=> (x = 3)
  } for 4 Int is sat
  legit_ite_noOverflow_4: {
    some x: Int | ((add[x,1] < 0) or (add[x,1] >= 0)) => x = 3 else x = 7
  } for 4 Int is sat
  
  -- Test that overflow is indeed prevented
  overflow_excluded_4: {
    some x: Int | x = 7 and add[x,1] = -8
  } for 4 Int is unsat

  -- From the original bug-report Gist:  https://gist.github.com/tnelson/01e9c784dbdb6d70911f6fc325ca9d33

  gist_specific_scenario: {
    ((add[7,1] < 0) or (add[7,1] >= 0)) => false
  } for 4 Int is unsat
  gist_left_scenario: {
    add[7,1] < 0
  } for 4 Int is unsat
  gist_right_scenario: {
    add[7,1] >= 0
  } for 4 Int is unsat
  gist_antecedent_only: {
    some x: Int | not (add[x,1] < 0 or add[x,1] >= 0)
  } for 4 Int is unsat
  gist_contradiction: {
    some x: Int | (not add[x,1] < 0) and (not add[x,1] >= 0)
  } for 4 Int is unsat
  gist_no_int_op: {
    some x: Int | ((x < 0) or (x >= 0)) => false
  } for 4 Int is unsat
  gist_ite_version_a5: {
    some x: Int | (add[x,1] < 0) => false else false
  } for 4 Int is unsat

  -- Examples from the paper (https://groups.csail.mit.edu/sdg/pubs/2012/abz12-overflow.pdf).
  -- The behavior for these must remain invariant. 
  gist_paper_excluded_middle: {
    all x: Int | multiply[x, 2] < 0 or !(multiply[x, 2] < 0)
  } for 3 Int is checked
  gist_paper_plus_equal: {
    add[4, 5] = add[6, 3]
  } for 4 Int is checked
  gist_paper_plus_not_equal: {
    add[4, 5] != add[6, 3]
  } for 4 Int is checked
}

