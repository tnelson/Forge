#lang forge
option verbose 0
option run_sterling off

/*
  Regression tests for Alloy issue # 330, which impacts any tool built atop 
  Pardinus. The issue was a soundness problem: with overflow-prevention on,
  unsatisfiable queries involving implication could produce spurious instances.
  
  If issues occur, some things to keep in mind. In Pardinus:
    * `BooleanMatrix` and `Int` carry a `defCond()` field, which has:
      * `getOverflow()` (did the operation itself overflow?)
      * `getAccumOverflow()` (did this operation, or any operation beneath it, overflow?)
      * `vars()` (which quantified variables did the comparison depend on?)
    * This field is populated by arithmetic, e.g., TwosComplementInt.plus. 
    * Formula atoms `in`, `=`, multiplicity checks, and integer comparisons invoke 
    `DefCond.ensureDef` to affect the resulting truth value. 
      * Thus, the field is not propagated into the Formula context, only the Expression context.
      * The resulting truth value depends on how many negations, and which kinds of quantifier  
      (over Int, or a subset of Int) are above the current formula. The domain of a quantifier 
      depends on which instantiated bodies are defined:
          - E.g., `all x: Int | add[x, 1] > x` is satisfiable because the instantiated 
          add[MAX,1] is excluded from the domain. 
          
   Core issues notes, as of 09-nov-2026:
     - the negation-polarity flag is not set in the IMPLIES case in Kodkod's translator. 
       this caused the issues with implies. 
     - fixing the above does not fix the expression-valued ITE case, which experiences similar issues. 
       note that the fix must also handle expression-valued ITE when enclosed in negation.
     - `DefCond.isInt` string-matches against `ints`: any set-of-integers other than this will not
       exhibit the same overflow-protection semantics. E.g., `all x : 7 | add[x,1] > x` should be 
       satisfiable since the universal is vacuously true. 
          
      
 
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
  // Design choice per Alloy issue
  // ite_expr_noOverflow_4: {
  //   some x: Int | some ((add[x,1] < 0 or add[x,1] >= 0) => none else x)
  // } for 4 Int is unsat

  -- Test expression-ITE under negation, since the semantics of overflow-prevention involves flipping polarity.
  ite_expr_negated_noOverflow_4: {
    some x: Int | not (((add[x,1] < 0 or add[x,1] >= 0) => sing[1] else sing[2]) = sing[1])
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

  -- Ensure that universals work correctly: bindings where the body is undefined are excluded. 
  // Related to the Alloy-issue design question, but lower-level (does Forge pass `ints` or `Int`?)
  // universal_noOverflow_4: {
  //   all x: Int | add[x,1] > x
  // } for 4 Int is sat

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

  -- According to the paper, the domain of universal quantifiers over Int is restricted 
  -- to the subset on which the quantifier's body is defined. This should be extended 
  -- to work on all integer-subsets:

  // Design choice per Alloy issue
  // non_literal_int_domain_vacuous: {
  //   all x: 7 | add[x, 1] > x
  // } for 4 Int is sat
  // non_literal_int_domain_singleton_works: {
  //   all x: 6+7 | add[x, 1] > x
  // } for 4 Int is sat
  non_literal_int_domain_control_no_overflow: {
    all x: 6 | add[x, 1] > x
  } for 4 Int is sat
}



