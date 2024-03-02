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
  foo: {(sum x : age | x.age) = 1} is forge_error
  -- test for bad decl arity in quantification
  bar: {some x : age | some x} is sat //forge_error
}