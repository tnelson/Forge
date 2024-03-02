#lang forge 

-- Tests for some errors of last resort in the AST; some of these 
-- will be regression tests.

one sig Person {
    age: one Int
}

test expect {
  -- Regression test for lack of arity-checking in implicit node/expr->node/int
  implicit_expr_to_intexpr_arity_add_1: {add[age, 1] = 1} is forge_error
  implicit_expr_to_intexpr_arity_add_2: {add[1, age] = 1} is forge_error
  -- Test for lack of arity-checking in direct use of sum
  sum_arity: {sum[age] = 1} is forge_error
  -- test for bad decl arity in sum aggregator
  sum_aggregator_decl_arity: {(sum x : age | x.age) = 1} is forge_error
  -- test for bad type in sum aggregator decl
  sum_aggregator_decl_fmla: {(sum x : (Person = Person) | x.age) = 1} is forge_error
  -- test for bad decl arity in quantification
  quantifier_decl_arity: {some x : age | some x} is forge_error
  -- test for bad type in quantifier decl 
  quantifier_decl_arity: {some x : (Person = Person) | some x} is forge_error
}