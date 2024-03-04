#lang forge 
option verbose 0
option run_sterling off

-- Tests for some errors of last resort in the AST; some of these 
-- will be regression tests.

one sig Person {
    age: one Int
}

test expect {
  ----- sum aggregator and ints -----

  -- Regression test for lack of arity-checking in implicit node/expr->node/int
  implicit_expr_to_intexpr_arity_add_1: {add[age, 1] = 1} is forge_error
  implicit_expr_to_intexpr_arity_add_2: {add[1, age] = 1} is forge_error
  -- (There is no comparable arity check for node/int -> node/expr)

  -- Test for lack of arity-checking in direct use of sum
  sum_arity: {sum[age] = 1} is forge_error
  -- test for bad decl arity in sum aggregator
  sum_aggregator_decl_arity: {(sum x : age | x.age) = 1} is forge_error
  -- test for bad type in sum aggregator decl
  sum_aggregator_decl_fmla: {(sum x : (Person = Person) | x.age) = 1} is forge_error
  -- test for bad arity in sum-aggregator sub-expression
  sum_aggregator_decl_fmla: {(sum x : Person | age) = 1} is forge_error


  ----- quantification -----
  -- Should be similar structure for each quantifier. Labeling the "some" cases for reference.
  
  -- SOME
  -- test for bad decl arity in quantification
  quantifier_some_decl_arity: {some x : age | some x} is forge_error
  -- test for bad syntax-type in quantifier decl 
  quantifier_some_decl_syntax_type: {some x : (Person = Person) | some x} is forge_error
  -- test for bad type in quantified-formula subformula
  quantifier_some_subformula_type: {some x : Person | x} is forge_error
  -- test for non-variable in quantifier variable position
  quantifier_some_decl_arity: {some Person : Person | some Person} is forge_error

  -- ALL
  quantifier_all_decl_arity: {all x : age | some x} is forge_error
  quantifier_all_decl_syntax_type: {all x : (Person = Person) | some x} is forge_error
  quantifier_all_subformula_type: {all x : Person | x} is forge_error
  quantifier_all_decl_arity: {all Person : Person | some Person} is forge_error

  -- LONE
  quantifier_lone_decl_arity: {lone x : age | some x} is forge_error
  quantifier_lone_decl_syntax_type: {lone x : (Person = Person) | some x} is forge_error
  quantifier_lone_subformula_type: {lone x : Person | x} is forge_error
  quantifier_lone_decl_arity: {lone Person : Person | some Person} is forge_error

  -- ONE
  quantifier_one_decl_arity: {one x : age | some x} is forge_error
  quantifier_one_decl_syntax_type: {one x : (Person = Person) | some x} is forge_error
  quantifier_one_subformula_type: {one x : Person | x} is forge_error
  quantifier_one_decl_arity: {one Person : Person | some Person} is forge_error

  -- NO
  quantifier_no_decl_arity: {no x : age | some x} is forge_error
  quantifier_no_decl_syntax_type: {no x : (Person = Person) | some x} is forge_error
  quantifier_no_subformula_type: {no x : Person | x} is forge_error
  quantifier_no_decl_arity: {no Person : Person | some Person} is forge_error


}