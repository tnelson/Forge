#lang racket/base

;; Error tester
;;
;; How to use:
;;  1. Make a new file* that gives an error at compile- or run-time
;;  2. Add the file's name to the REGISTRY below
;;  3. Also add a regular expression or predicate** to the REGISTRY
;;  4. Run this file (main.rkt) to test
;;
;; ** Works the same way as RackUnit's check-exn <https://docs.racket-lang.org/rackunit/api.html>A

(require
  rackunit
  (only-in rackunit/text-ui run-tests)
  racket/runtime-path
  racket/port)

(define-runtime-path here ".")

;; -----------------------------------------------------------------------------

(define REGISTRY
  (list
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ; Some error tests look at the specific source-location blamed
   
    ; misuse of predicates and helper functions with arguments/no-arguments
    (list "expect-predicate-args.frg" #rx"Element 1 of this block was an ill-formed")
    (list "expect-predicate-no-args.frg" #rx"expect-predicate-no-args.frg:13:45.*Tried to give arguments to a predicate, but it takes none")

    ; TODO: needs switch to raise-forge-error so the proper location is in the message
    ; (list "expect-fun-args.frg" #rx"Racket procedure, which is likely.*expect-fun-args.frg:11")
    ; TODO: needs to confirm that equality is irrespective of source location (it should be)
    ;(list "expect-fun-no-args.frg" #rx"TODO")

    ;;;;;;;;;;;;;;;;;;;;;;;;

    (list "./malformed-test-double.frg" #rx"FOR-TOK") ;; regression
    
    ;;;;;;; Source locations ;;;;;;;
    (list "./loc/sig_use_loc_error.frg" #rx"sig_use_loc_error.frg:7:39") ; vs. reachable
    (list "./loc/field_use_loc_error.frg" #rx"field_use_loc_error.frg:7:29")   ; vs. reachable
    (list "./loc/ifte-error.frg" #rx"ifte-error.frg:9:5.*If-then-else needed consistent types")  
   
    (list "piecewise-bind-repeat.frg" #rx"rebinding detected")
    (list "piecewise-bind-combine.frg" #rx"may not be combined with complete bounds")
    (list "piecewise-bind-sigs.frg" #rx"would create a relation")
    (list "piecewise-bind-mix-ops.frg" #rx"mixed operators not allowed")
    (list "piecewise_domain_too_big.frg" #rx"Field spouse was bounded for atom")
   
    (list "hidden-wheat.frg" #rx"Invalid binding expression")
    (list "unstated_bounds.frg" #rx"Scope for Match was not declared")
    (list "multiple-positive-examples-failing.frg" #rx"Invalid example 'e1'.*?Invalid example 'e2'") 
   
    (list "abstract.frg" #rx"abstract")
    (list "bsl-ast-arrow.frg" #rx"Direct use of ->")
    (list "bsl-ast-intersect.frg" #rx"& operator")
    (list "bsl-ast-minus.frg" #rx"- operator")
    (list "bsl-ast-plus.frg" #rx"\\+ operator")
    (list "bsl-ast-star.frg" #rx"\\* operator")
    (list "bsl-ast-transitive-closure.frg" #rx"\\^ operator")
    (list "bsl-arrow.frg" #rx"Direct use of ->")
    (list "bsl-int-minus.frg" #rx"- operator")
    (list "bsl-join-right.frg" #rx"not an object")
    (list "bsl-join-right2.frg" #rx"not an object")
    (list "bsl-join-right3.frg" #rx"not an object")
    (list "bsl-join-right4.frg" #rx"not an object")
    (list "bsl-join-right5.frg" #rx"not an object")
    (list "bsl-join-right6.frg" #rx"not an object")
    (list "bsl-join.frg" #rx"not an object")
    (list "bsl-join2.frg" #rx"did not result in a singleton value")

    (list "bsl-join3.frg" #rx"not an object")
    (list "bsl-reachable.frg" #rx"field")
    (list "bsl-reachable2.frg" #rx"not a singleton")
    (list "bsl-set-singleton-equal.frg" #rx"singleton")
    (list "bsl-set.frg" #rx"not a singleton")
    (list "bsl-transpose-ast.frg" #rx"~ operator")

    (list "bsl-reachable-nonfield-varargs.frg" #rx"Field argument given to reachable is not a field")
    (list "bsl-reachable-nonfield.frg" #rx"Field argument given to reachable is not a field")
    (list "bsl-reachable-too-few-arguments.frg" #rx"The reachable predicate expected at least three arguments, given 2")

;    (list "froglet-ast-arrow.frg" #rx"-> operator is not")
    (list "froglet-ast-bind.frg" #rx"bind is not part of the language")
;    (list "froglet-ast-intersect.frg" #rx"& operator is not")
;    (list "froglet-ast-minus.frg" #rx"- operator is not")
;    (list "froglet-ast-plus.frg" #rx"\\+ operator is not")
;    (list "froglet-ast-star.frg" #rx"\\* operator is not")
;    (list "froglet-ast-transitive-closure.frg" #rx"\\^ operator is not")
;    (list "froglet-ast-transpose.frg" #rx"~ operator is not")
;    (list "froglet-ast-notimplies.frg" #rx"parsing error")
    (list "froglet-arrow.frg" #rx"-> operator is not")
;    (list "froglet-compareop0.frg" #rx"inputs to = must have similar type")
;    (list "froglet-compareop1.frg" #rx"inputs to = must have similar type")
;    (list "froglet-compareop2.frg" #rx"expected an Int")
;    (list "froglet-compareop3.frg" #rx"expected an Int")
    (list "froglet-http.frg" #rx"field declaration")
    (list "froglet-int-minus.frg" #rx"- operator is not")
;    (list "froglet-field-shadow.frg" #rx"expected a field")
;    (list "froglet-is-linear0.frg" #rx"outside of bounds block")
;    (list "froglet-is-linear1.frg" #rx"expected a field")
;    (list "froglet-join-right.frg" #rx"not an object")
;    (list "froglet-join-right2.frg" #rx"not an object")
;    (list "froglet-join-right3.frg" #rx"not an object")
;    (list "froglet-join-right4.frg" #rx"not an object")
;    (list "froglet-join-right5.frg" #rx"not an object") ;;(#rx"Expected a singleton sig") ;; TODO is not a Node
;    (list "froglet-join-right6.frg" #rx"not an object")
;    (list "froglet-join.frg" #rx"not an object")
;    (list "froglet-join2.frg" #rx"not a singleton") ;; TODO incomplete or partial?
;    (list "froglet-join3.frg" #rx"not an object")
;    (list "froglet-no-field.frg" #rx"expected an object")
;    (list "froglet-pred.frg" #rx"expected a formula")
;    (list "froglet-reachable1.frg" #rx"no field match")
;    (list "froglet-reachable2.frg" #rx"expected an object")
;    (list "froglet-reachable3.frg" #rx"parsing error")
;    (list "froglet-reachable4.frg" #rx"reachable expects 2 or more arguments")
;    (list "froglet-reachable5.frg" #rx"reachable found no path to target")
;    (list "froglet-reachable6.frg" #rx"reachable cannot use field")
;    (list "froglet-set-singleton-equal.frg" #rx"not a singleton") ;;(#rx"= expects two objects, sig Node is not an object")
;    (list "froglet-set.frg" #rx"not a singleton") #;( "pred must return an object, not a set")
;    (list "froglet-uni-0.frg" #rx"expected an object")
;    (list "froglet-uni-1.frg" #rx"expected an object")
;    (list "froglet-uni-2.frg" #rx"expected an object") ;; or, a singleton
;    (list "froglet-uni-3.frg" #rx"expected an object")
;    (list "froglet-uni-4.frg" #rx"expected a field")
;    (list "froglet-uni-5.frg" #rx"expected a field")
;    (list "froglet-pubkey2.frg" #rx"field 'k' not found")

    ;; challenge
    ;; - [X] pubkey2 froglet expected a field got k
    ;;   ... lookup works for `pairs`, not for `k`,
    ;;       should find the k parameter in the environment!
    ;; - [X] expected a formula (why?!) given a name in k
    ;; - [X] pubkey2: why unknown type for 1st branch
    ;; - [ ] pubkey2
    ;;   froglet: binop-check: (#<syntax in> '#s(type #<syntax Unknown>)
    ;;   '#s((nametype type 1) #<syntax:pubkey2.frg:30:34 Pubk>))
    ;; - [ ] pubkey2: refine branch type
    ;; - [ ] pubkey2: is function type parsing broken?
    ;; TODO catch all bsl errors from here
    ;; TODO check Ed for bounds errors
    ;; TODO modeling in anger ... invalid bounds

    ;; FYI forge has an evaluator, used to eval in inst specs, typechecker may need it too!

    (list "example_electrum.frg" #rx"example foo: .* temporal")
    (list "example_impossible.frg" #rx"Invalid example 'onlyBabies'")
    (list "excluded-extender-value.frg" #rx"not a subset")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_theorem.frg" #rx"failed.")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_unsat.frg" #rx"Failed test")
    (list "failed_sat.frg" #rx"Failed test") 
    ;;; ? after * makes the match lazy, meaning it will match as few characters as possible while still allowing the remainder of the regular expression to match.
    

    ;;;; TODO: Change these patterns!
    (list "multiple_test_failures.frg" #rx".*?Assertion_All_isRoot_is_necessary_for_isNotRoot failed.*?Invalid example 'thisIsNotATree'.*?Test t1 failed")
    (list "properties_undirected_tree_underconstraint_multiple_errors.frg" #rx".*?necessary_assertion_for_isUndirectedTree_.*? failed.*?quantified_necessary_assertion_for_isUndirectedTree.*?failed")
    (list "properties_undirected_tree_overconstraint_error.frg" #rx"Assertion_isUndirected_is_sufficient_for_isUndirectedTree failed.")
    (list "properties_directed_tree_sufficiency_error.frg" #rx"Assertion_All_arethesame_is_sufficient_for_bothRoots failed.")
    (list "consistency-error.frg" #rx"Failed test Assert_consistent_q_")
    (list "inconsistency-error.frg" #rx"Failed test Assert_inconsistent_q_")
    (list "properties_directed_tree_necessity_error.frg" #rx"Assertion_All_isRoot_is_necessary_for_isNotRoot failed.")



    (list "formula_comprehension_cardinality.frg" #rx"expected to be given")
    (list "formula_comprehension_multiplicity.frg" #rx"expected to be given")
    (list "hello.frg" #rx"parsing error")
    (list "bsl_multiple_failures.frg" #rx".*?Failed test t1.*?Failed test t2")
    
    (list "ill_typed_inst_columns_reversed.frg" #rx"age")
    (list "inst-undefined-bound-child-one.frg" #rx"for an ancestor of")
    (list "inst-lower-not-subset-upper.frg" #rx"was not a superset")
    (list "invalid-example.frg" #rx"Invalid example 'onlyBabies'")

    (list "no-temporal-ltl.frg" #rx"use of LTL operator without temporal problem_type")
    (list "no-temporal-ltl-evaluator.frg" #rx"use of LTL operator without temporal problem_type")

    (list "expr-in-comprehension-condition.frg" #rx"expected a formula")
    (list "non-expr-in-comprehension-domain.frg" #rx"expected a singleton or relational expression")
    (list "arity-in-comprehension-domain.frg" #rx"expected a singleton or relational expression of arity 1")

    (list "override-wrong-arity.frg" #rx"must have same arity")
    (list "override-no-overlap.frg" #rx"will never override anything")

    (list "run-given-non-formula.frg" #rx"Expected a formula but got something else")

    (list "int_literal_too_big.frg" #rx"could not be represented in the current bitwidth")

    (list "parsing_less_dash.frg" #rx"Negative numbers must not have blank space between the minus")

    ;; Mismatched type tests - pred
    (list "mismatched-arg-type-basic.frg" #rx"Argument 1 of 1 given to predicate p2 is of incorrect type. Expected type \\(B\\), given type \\(A\\)")
    (list "mismatched-arg-type-basic-univ.frg" #rx"Argument 1 of 1 given to predicate p2 is of incorrect type. Expected type \\(B\\), given type univ") 
    (list "mismatched-arg-type-arity.frg" #rx"Argument 2 of 2 given to predicate p2 is of incorrect type. Expected type \\(B\\), given type \\(A\\)")
    (list "mismatched-arg-type-no-quant.frg" #rx"Argument 1 of 1 given to predicate p1 is of incorrect type. Expected type \\(A\\), given type \\(C\\)")
    (list "mismatched-arg-type-no-quant2.frg" #rx"Argument 2 of 2 given to predicate p2 is of incorrect type. Expected type \\(B\\), given type \\(A\\)")
    (list "mismatched-arg-type-non-primsig.frg" #rx"Argument 1 of 1 given to predicate p2 is of incorrect type. Expected type \\(B\\), given type \\(A\\)")
    (list "mismatched-arg-type-non-primsig2.frg" #rx"Argument 1 of 1 given to predicate p is of incorrect type. Expected type \\(C\\), given type \\(B\\)")
    (list "tree-type-error.frg" #rx"Argument 1 of 1 given to predicate p is of incorrect type. Expected type \\(A B_child1\\), given type \\(B_child2\\)")
    (list "mismatched-arg-type-int.frg" #rx"Argument 1 of 1 given to predicate p2 is of incorrect type. Expected type \\(Int\\), given type \\(A\\)")

    ;; Mismatched type tests - fun
    (list "mismatched-arg-type-fun.frg" #rx"Argument 1 of 1 given to function f is of incorrect type. Expected type \\(A\\), given type \\(B\\)")
    (list "mismatched-arg-type-fun-arity.frg" #rx"Argument 2 of 2 given to function f is of incorrect type. Expected type \\(A\\), given type \\(B\\)")
    (list "mismatched-arg-type-fun-output.frg" #rx"The output of function f is of incorrect type")
    (list "mismatched-arg-type-fun-codomain.frg" #rx"The output of function f is of incorrect type")
    (list "mismatched-arg-type-fun-codomain-non-primsig.frg" #rx"The output of function f is of incorrect type")
    (list "mismatched-arg-type-fun-output-non-primsig.frg" #rx"The output of function f is of incorrect type")
    (list "mismatched-arg-type-fun-univ-output.frg" #rx"The output of function f is of incorrect type")
    (list "mismatched-arg-type-fun-output-int.frg" #rx"The output of function f is of incorrect type")
    (list "mismatched-arg-type-fun-codomain-int.frg" #rx"The output of function f is of incorrect type")

    ;; priming error tests
    (list "priming-basic.frg" #rx"Prime operator used in non-temporal context")

    ;; The "is theorem" test construct is temporarily disabled in favor of "is checked";
    ;; it will be re-enabled for complete solver backends only
    (list "is_theorem_disabled.frg" #rx"use 'is checked' instead")
  ))


;; -----------------------------------------------------------------------------

(define (run-error-tests registry)
  (printf "Error Tester: running ~s tests~n" (length registry))
  (for/and ((test+pred* (in-list registry)))
    (define test-name (car test+pred*))
    (define pred (cadr test+pred*))
    (printf "run test: ~a~n" test-name)
    
    
    (with-check-info*
      (list (make-check-name test-name))
      (lambda ()
        (check-exn pred (lambda ()
                          (define mocked-stderr (open-output-string))
                          (parameterize ([current-error-port mocked-stderr])
                            ; run test-name, and if a non-exception was raised, raise it as a user-error
                            (re-raise-strings (run-error-test test-name))
                            ; If we reach this point, no exception was raised. Thus, look in mocked stderr
                            (re-raise-strings (raise (get-output-string mocked-stderr)))
                            ; will also fail b/c not exception
                            )))))
    (void)))


(define (run-error-test test-name)
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-directory here])
    (let* ([root-module `(file ,(path->string (build-path here test-name)))])
      (dynamic-require root-module #f)
      (dynamic-require `(submod ,root-module execs) #f))))

(define-syntax-rule (re-raise-strings expr)
  (with-handlers ([string? raise-user-error]) expr))

;; -----------------------------------------------------------------------------

(module+ main
  (unless (zero?
            (run-tests
              (test-suite "error/main.rkt"
                (test-case "error tests"
                  (run-error-tests REGISTRY)))))
    (exit 1)))


