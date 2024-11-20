#lang forge/temporal

-- Do not decrease the verbosity. This ensures certain checks are run. 
option verbose 1
option run_sterling off

-- Tests for some errors of last resort in the AST; some of these 
-- will be regression tests.

sig Person {
    age: one Int
}
one sig Nim extends Person {}

test expect {
  ----- sum aggregator and ints -----

  -- Regression test for lack of arity-checking in implicit node/expr->node/int
  implicit_expr_to_intexpr_arity_add_1: {add[age, 1] = 1} 
    is forge_error "Could not treat age as an integer expression."
  implicit_expr_to_intexpr_arity_add_2: {add[1, age] = 1} 
    is forge_error "Could not treat age as an integer expression."
  -- (There is no comparable arity check for node/int -> node/expr)

  -- Test for lack of arity-checking in direct use of sum
  sum_arity: {sum[age] = 1} 
    is forge_error "1 of 1 to sum was not expression with arity 1"
  -- test for bad decl arity in sum aggregator
  sum_aggregator_decl_arity: {(sum x : age | x.age) = 1} 
    is forge_error "sum aggregator expected its declaration expression to have arity 1"
  -- test for bad type in sum aggregator decl
  sum_aggregator_decl_domain: {(sum x : (Person = Person) | x.age) = 1} 
    is forge_error "sum aggregator expected an expression in its declaration"
  -- test for bad arity in sum-aggregator sub-expression
  sum_aggregator_decl_fmla: {(sum x : Person | age) = 1} 
    is forge_error "Could not treat age as an integer expression"
  -- test that the sum aggregator gives an error when multiple variables are declared
  sum_aggregator_multiple_decls: {(sum x: Person, y: Person | min[x.age + y.age]) = 1} 
    is forge_error "sum aggregator only supports a single variable"
  -- but that nesting is OK
  sum_aggregator_multiple_decls_nested_ok: {(sum x: Person | sum y: Person | min[x.age + y.age]) = 1} 
    is sat
  
  ----- quantification -----
  -- Should be similar structure for each quantifier. Labeling the "some" cases for reference.


  -- SOME
  -- test for bad decl arity in quantification
  quantifier_some_decl_arity: {some x : age | some x} 
    is forge_error "some quantifier expected an arity-1 expression for domain"
  -- test for bad syntax-type in quantifier decl   
  quantifier_some_decl_syntax_type: {some x : (Person = Person) | some x} 
    is forge_error "some quantifier expected an expression for domain"  
  -- test for bad type in quantified-formula subformula
  quantifier_some_subformula_type: {some x : Person | x} 
    is forge_error "some quantifier body expected a formula"

  -- ALL
  quantifier_all_decl_arity: {all x : age | some x} 
    is forge_error "all quantifier expected an arity-1 expression for domain"
  quantifier_all_decl_syntax_type: {all x : (Person = Person) | some x} 
    is forge_error "all quantifier expected an expression for domain"
  quantifier_all_subformula_type: {all x : Person | x} 
    is forge_error "all quantifier body expected a formula"

  -- LONE (current errors not perfect, but better than contract violations)
  quantifier_lone_decl_arity: {lone x : age | some x}
    is forge_error "Set-comprehension variable domain expected a singleton or relational"
  quantifier_lone_decl_syntax_type: {lone x : (Person = Person) | some x} 
    is forge_error "Set-comprehension variable domain expected a singleton or relational"
  quantifier_lone_subformula_type: {lone x : Person | x} 
    is forge_error "Set-comprehension condition expected a formula"

  -- ONE
  quantifier_one_decl_arity: {one x : age | some x}
    is forge_error "Set-comprehension variable domain expected a singleton or relational"
  quantifier_one_decl_syntax_type: {one x : (Person = Person) | some x} 
    is forge_error "Set-comprehension variable domain expected a singleton or relational"
  quantifier_one_subformula_type: {one x : Person | x}
    is forge_error "Set-comprehension condition expected a formula"
  
  -- NO (again, ideally this would say *NO* quantifier, not *SOME* quantifier)
  quantifier_no_decl_arity: {no x : age | some x} 
    is forge_error "quantifier expected an arity-1 expression for domain"
  quantifier_no_decl_syntax_type: {no x : (Person = Person) | some x} 
    is forge_error "quantifier expected an expression for domain"
  quantifier_no_subformula_type: {no x : Person | x}
    is forge_error "quantifier body expected a formula"

  -- Formula operators mis-used
  and_given_expr_1: {some Person and Person} 
    is forge_error "argument 2 of 2 to && had unexpected type"
  or_given_expr_1: {some Person or Person} 
    is forge_error "argument 2 of 2 to || had unexpected type"
  not_given_expr: {not Person} is forge_error
  implies_given_expr_1: {some Person implies Person} 
    is forge_error "argument 2 of 2 to => had unexpected type"
  iff_given_expr_1: {some Person iff Person} 
    is forge_error "argument 2 of 2 to => had unexpected type"
  -- other direction for binary operators
  and_given_expr_2: {Person and some Person} 
    is forge_error "argument 1 of 2 to && had unexpected type"
  or_given_expr_2: {Person or some Person} 
    is forge_error "argument 1 of 2 to || had unexpected type"
  implies_given_expr_2: {Person implies some Person} 
    is forge_error "argument 1 of 2 to => had unexpected type"
  iff_given_expr_2: {Person iff some Person} 
    is forge_error "argument 1 of 2 to => had unexpected type"

  -- Relational operators mis-used
  join_given_formula_1: {some (some Nim).Nim} 
    is forge_error "join operator expected to be given"
  join_given_formula_2: {some Nim.(some Nim)} 
    is forge_error "join operator expected to be given"
  boxjoin_given_formula_1: {some (some Nim)[Nim]} 
    is forge_error "Could not use a boolean-valued formula as a predicate, function, or field name"
  boxjoin_given_formula_2: {some Nim[(some Nim)]} 
    is forge_error "join operator expected to be given an atom- or set-valued expression"
  union_given_formula_1: {some (some Nim) + Nim} 
    is forge_error "\+ operator expected to be given an atom- or set-valued expression"
  union_given_formula_2: {some Nim + (some Nim)} 
    is forge_error "\+ operator expected to be given an atom- or set-valued expression"
  intersect_given_formula_1: {some (some Nim) & Nim} 
    is forge_error "& operator expected to be given an atom- or set-valued expression"
  intersect_given_formula_2: {some Nim & (some Nim)} 
    is forge_error "& operator expected to be given an atom- or set-valued expression"
  subtract_given_formula_1: {some (some Nim) - Nim} 
    is forge_error "- operator expected to be given an atom- or set-valued expression"
  subtract_given_formula_2: {some Nim - (some Nim)} 
    is forge_error "- operator expected to be given an atom- or set-valued expression"
  product_given_formula_1: {some (some Nim) -> Nim} 
    is forge_error "-> operator expected to be given an atom- or set-valued expression"
  product_given_formula_2: {some Nim -> (some Nim)} 
    is forge_error "-> operator expected to be given an atom- or set-valued expression"
  tc_given_formula: {some ^(some Person)} 
    is forge_error "\^ operator expected to be given an atom- or set-valued expression"
  rtc_given_formula: {some *(some Person)} 
    is forge_error "\* operator expected to be given an atom- or set-valued expression"
  transpose_given_formula: {some ~(some Person)} 
    is forge_error "~ operator expected to be given an atom- or set-valued expression"

  -- Set comprehension mis-used 
  comprehension_used_quantifier: { some {all x: Person | some x.age}} is forge_error
  comprehension_bad_decl_arity: { some {x: age | some x.age}} is forge_error
  comprehension_bad_decl_type: { some {x: (Person = Person) | some x.age}} is forge_error

  -- Cardinality mis-used
  card_given_fmla: { #(Person = Person) = 1 } is forge_error

  -- Temporal formula operators mis-used
  eventually_given_expr: {eventually Person} is forge_error
  always_given_expr: {always Person} is forge_error
  next_state_given_expr: {next_state Person} is forge_error
  until_given_expr_1: {Person until some Person} is forge_error
  until_given_expr_2: {some Person until Person} is forge_error
  releases_given_expr_1: {Person releases some Person} is forge_error
  releases_given_expr_2: {some Person releases Person} is forge_error
  historically_given_expr: {historically Person} is forge_error
  once_given_expr: {once Person} is forge_error
  prev_state_given_expr: {prev_state Person} is forge_error
  since_given_expr_1: {Person since some Person} is forge_error
  since_given_expr_2: {some Person since Person} is forge_error
  triggered_given_expr_1: {Person triggered some Person} is forge_error
  triggered_given_expr_2: {some Person triggered Person} is forge_error

  ----- Errors possibly from outside the AST -----

  -- Join type mismatch
  empty_join_result: {some age.Nim} is forge_error
  -- 0-ary join result
  zero_arity_join_result: {some Nim.Nim} is forge_error


  --------------------------------------------------------------------
  -- Variable-name shadowing: this is an ergonomic issue. In basic examples, the need for
  -- an error is "obvious". But even the Forge developers re-use variable names in helper
  -- predicates, and so a very strict shadowing check would be annoying at best. We're
  -- sticking with shadowing checks on *identity*, rather than *user-facing name*.
  -- Thus, these tests should *run*, not produce an error.
  -- Shadowing across quantification
  OK_variable_name_shadowing: { some x: Person | all x: Person | some x } is sat
  -- Variable-name shadowing between quantification and comprehension
  OK_quant_comp_variable_shadowing: {some {x : Person | some x: Person | some x.age}} is sat
  -- Same as above, but in the other direction  
  OK_comp_quant_variable_shadowing: {some x: Person | some {x : Person | some x.age}} is sat
  -- Variable-name shadowing between nested comprehensions
  OK_nested_comp_variable_shadowing: {some {x: Person | some {x: Person | some x.age} }} is sat

  -- However, shadowing by name is disallowed within a single construct
  -- Regression test: shadowing within a *single* comprehension or quantifier would cause Pardinus to crash.
  internal_comp_variable_name_shadowing: {some {x: Person, x: Person | x.age = x.age}} is forge_error
  internal_quant_variable_name_shadowing: {some x: Person, x: Person | x.age = x.age} is forge_error

  --------------------------------------------------------------------

  -- Regression test: checker must descend into RHS of a minus, even if the RHS has no impact
  -- on the inferred type of the expression.
  set_minus_rhs_empty_join: { some Person - (age.Nim) } is forge_error

  -- Minus operator: check RHS for validity, allow chaining
  -- (Does not apply to sum aggregator, since that requires only one decl)
  set_minus_rhs_quant_all: {all x: Person, y: Person - x | x != y} is checked -- should be OK
  set_minus_rhs_quant_no: {no x: Person, y: Person - x | x != y} is sat     -- should be OK
  set_minus_rhs_quant_some: {some x: Person, y: Person - x | x != y} is sat -- should be OK
  set_minus_rhs_quant_lone: {lone x: Person, y: Person - x | x != y} is sat -- should be OK
  set_minus_rhs_quant_one: {one x: Person, y: Person - x | x != y} is unsat -- should be OK
  set_minus_rhs_comp: {some {x: Person, y: Person - x | x != y}} is sat     -- should be OK

  -- test for non-variable in quantifier variable position
  quantifier_some_decl_var: {some Person : Person | some Person} is forge_error
  quantifier_all_decl_var: {all Person : Person | some Person} is forge_error
  quantifier_lone_decl_var: {lone Person : Person | some Person} is forge_error
  quantifier_one_decl_var: {one Person : Person | some Person} is forge_error
  quantifier_no_decl_var: {no Person : Person | some Person} is forge_error
  comprehension_bad_decl_var: { some {Person: Person | some Person.age}} is forge_error
  -- regression test: the above "sig name as variable" forms could cause the below to crash:
  all_multiple_vars_ok: {all x: Person, y: Person | x != y} is unsat -- should be OK
}

-- To be integrated with next phase of last-checker improvements

-- Parse error 
--fun primeAfterBoxNoGroup[x: Thread]: set Location {World.loc[x]'}

sig Location {}
sig Thread {}
one sig World {var loc: func Thread -> Location}
fun baseline[x: Thread]: set Location {World.loc'[x]}

test expect {
    non_equiv1: {some x: Thread | baseline[x] != World'.loc[x]} 
      is forge_error "Prime operator used in non-temporal context"
    non_equiv2: {some x: Thread | baseline[x] != World.loc[x']} 
      is forge_error "Prime operator used in non-temporal context"
    non_equiv3: {some x: Thread | baseline[x] != (World.loc)'[x]} 
      is unsat
    -- Nothing to do with priming; this one should give an empty-join error
    non_equiv4: {some x: Thread | baseline[x] != World.(loc[x])} 
      is forge_error
    non_equiv5: {some x: Thread | baseline[x] != (World.loc[x])'} 
      is unsat
}
