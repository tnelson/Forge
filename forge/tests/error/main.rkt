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
  racket/runtime-path)

(define-runtime-path here ".")

;; -----------------------------------------------------------------------------

(define REGISTRY
  (list
    (list "piecewise-bind-repeat.frg" #rx"rebinding detected")
    (list "piecewise-bind-combine.frg" #rx"may not be combined with complete bounds")
    (list "piecewise-bind-sigs.frg" #rx"would create a relation")
    (list "piecewise-bind-mix-ops.frg" #rx"mixed operators not allowed")
   
    (list "hidden-wheat.frg" #rx"Invalid binding expression")
   
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
    (list "bsl-join2.frg" #rx"not a singleton")
    (list "bsl-join3.frg" #rx"not an object")
    (list "bsl-reachable.frg" #rx"field")
    (list "bsl-reachable2.frg" #rx"not a singleton")
    (list "bsl-set-singleton-equal.frg" #rx"singleton")
    (list "bsl-set.frg" #rx"not a singleton")
    (list "bsl-transpose-ast.frg" #rx"~ operator")

    (list "bsl-reachable-nonfield-varargs.frg" #rx"Field argument given to reachable is not a field")
    (list "bsl-reachable-nonfield.frg" #rx"Field argument given to reachable is not a field")
    
    (list "froglet-ast-arrow.frg" #rx"-> operator is not")
    (list "froglet-ast-bind.frg" #rx"bind is not part of the language")
    (list "froglet-ast-intersect.frg" #rx"& operator is not")
    (list "froglet-ast-minus.frg" #rx"- operator is not")
    (list "froglet-ast-plus.frg" #rx"\\+ operator is not")
    (list "froglet-ast-star.frg" #rx"\\* operator is not")
    (list "froglet-ast-transitive-closure.frg" #rx"\\^ operator is not")
    (list "froglet-ast-transpose.frg" #rx"~ operator is not")
    (list "froglet-arrow.frg" #rx"-> operator is not")
    (list "froglet-http.frg" #rx"field declaration")
    (list "froglet-int-minus.frg" #rx"- operator is not")
    (list "froglet-join-right.frg" #rx"not an object")
    (list "froglet-join-right2.frg" #rx"not an object")
    (list "froglet-join-right3.frg" #rx"not an object")
    (list "froglet-join-right4.frg" #rx"not an object")
    (list "froglet-join-right5.frg" #rx"not an object") ;;(#rx"Expected a singleton sig") ;; TODO is not a Node
    (list "froglet-join-right6.frg" #rx"not an object")
    (list "froglet-join.frg" #rx"not an object")
    (list "froglet-join2.frg" #rx"not a singleton") ;; TODO incomplete or partial?
    (list "froglet-join3.frg" #rx"not an object")
    (list "froglet-pred.frg" #rx"expected a formula")
    (list "froglet-reachable.frg" #rx"field") ;; TODO
    (list "froglet-reachable2.frg" #rx"not a singleton") ;; TODO
    (list "froglet-set-singleton-equal.frg" #rx"not a singleton") ;;(#rx"= expects two objects, sig Node is not an object")
    (list "froglet-set.frg" #rx"not a singleton") #;( "pred must return an object, not a set")
;    (list "froglet-uni-0.frg" #rx"expected a sig")
;    (list "froglet-uni-1.frg" #rx"expected a sig")
;    (list "froglet-uni-2.frg" #rx"is not a value") ;; TODO better block checking
;    (list "froglet-uni-3.frg" #rx"expected a sig")
;    (list "froglet-uni-4.frg" #rx"expected a sig") ;; TODO reachable, double-check

    (list "example_electrum.frg" #rx"example foo: .* temporal")
    (list "example_impossible.frg" #rx"Invalid example 'onlyBabies'")
    (list "excluded-extender-value.frg" #rx"not a subset")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_theorem.frg" #rx"failed.")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_unsat.frg" #rx"Failed test")
    (list "failed_sat.frg" #rx"Failed test") 
    (list "properties_undirected_tree_underconstraint_error.frg" #rx"Assertion_TreeWithEdges_is_necessary_for_isUndirectedTree failed.")
    (list "properties_undirected_tree_overconstraint_error.frg" #rx"Assertion_isUndirected_is_sufficient_for_isUndirectedTree failed.")
    (list "formula_comprehension_cardinality.frg" #rx"expected to be given")
    (list "formula_comprehension_multiplicity.frg" #rx"expected to be given")
    (list "hello.frg" #rx"parsing error")
    
    (list "ill_typed_inst_columns_reversed.frg" #rx"age")
    (list "inst-undefined-bound-child-one.frg" #rx"for an ancestor of")
    (list "inst-lower-not-subset-upper.frg" #rx"was not a superset")
    (list "invalid-example.frg" #rx"Invalid example 'onlyBabies'")

    (list "no-temporal-ltl.frg" #rx"use of LTL operator without temporal problem_type")
    (list "no-temporal-ltl-evaluator.frg" #rx"use of LTL operator without temporal problem_type")

    (list "expr-in-comprehension-condition.frg" #rx"expected a formula")
    (list "non-expr-in-comprehension-domain.frg" #rx"expected a singleton or relational expression")
    (list "arity-in-comprehension-domain.frg" #rx"variable domain needs arity = 1")
    (list "expect-predicate-args.frg" #rx"Ill-formed block")
    (list "expect-predicate-no-args.frg" #rx"Tried to give arguments to a predicate, but it takes none")

    (list "override-wrong-arity.frg" #rx"must have same arity")
    (list "override-no-overlap.frg" #rx"will never override anything")

    (list "run-given-non-formula.frg" #rx"Expected a formula but got something else") 
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
        (check-exn pred (lambda () (re-raise-strings (run-error-test test-name))))))
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


