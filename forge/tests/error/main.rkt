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
    (list "abstract.frg" #rx"abstract")
    (list "bsl-arrow-ast.frg" #rx"Direct use of ->")
    (list "bsl-arrow.frg" #rx"Direct use of ->")
    (list "bsl-int-minus.frg" #rx"- operator")
    (list "bsl-intersect-ast.frg" #rx"& operator")
    (list "bsl-join-right.frg" #rx"not an object")
    (list "bsl-join-right2.frg" #rx"not an object")
    (list "bsl-join-right3.frg" #rx"not an object")
    (list "bsl-join-right4.frg" #rx"not an object")
    (list "bsl-join-right5.frg" #rx"not an object")
    (list "bsl-join-right6.frg" #rx"not an object")
    (list "bsl-join.frg" #rx"not an object")
    (list "bsl-join2.frg" #rx"not a singleton")
    (list "bsl-join3.frg" #rx"not an object")
    (list "bsl-minus-ast.frg" #rx"- operator")
    (list "bsl-plus-ast.frg" #rx"\\+ operator")
    (list "bsl-reachable.frg" #rx"field")
    (list "bsl-reachable2.frg" #rx"not a singleton")
    (list "bsl-set-singleton-equal.frg" #rx"singleton")
    (list "bsl-set.frg" #rx"not a singleton")
    (list "bsl-star-ast.frg" #rx"\\* operator")
    (list "bsl-transitive-closure-ast.frg" #rx"\\^ operator")
    (list "bsl-transpose-ast.frg" #rx"~ operator")

    (list "froglet-arrow-ast.frg" #rx"Operator -> is not allowed")
    (list "froglet-arrow.frg" #rx"Operator -> is not allowed")
    (list "froglet-http.frg" #rx"field declaration")
    (list "froglet-int-minus.frg" #rx"Operator - is not allowed")
    (list "froglet-intersect-ast.frg" #rx"Operator & is not allowed")
    (list "froglet-join-right.frg" #rx"not an object")
    (list "froglet-join-right2.frg" #rx"not an object")
    (list "froglet-join-right3.frg" #rx"not an object")
    (list "froglet-join-right4.frg" #rx"not an object")
    (list "froglet-join-right5.frg" #rx"not an object") ;;(#rx"Expected a singleton sig") ;; TODO is not a Node
    (list "froglet-join-right6.frg" #rx"not an object")
    (list "froglet-join.frg" #rx"not an object")
    (list "froglet-join2.frg" #rx"not a singleton") ;; TODO incomplete or partial?
    (list "froglet-join3.frg" #rx"not an object")
    (list "froglet-minus-ast.frg" #rx"Operator -")
    (list "froglet-plus-ast.frg" #rx"Operator \\+")
    (list "froglet-reachable.frg" #rx"field") ;; TODO
    (list "froglet-reachable2.frg" #rx"not a singleton") ;; TODO
    (list "froglet-set-singleton-equal.frg" #rx"not a singleton") ;;(#rx"= expects two objects, sig Node is not an object")
    (list "froglet-set.frg" #rx"pred must return an object, not a set")
    (list "froglet-star-ast.frg" #rx"Operator \\*")
    (list "froglet-transitive-closure-ast.frg" #rx"Operator \\^")
    (list "froglet-transpose-ast.frg" #rx"Operator ~")
;    (list "froglet-uni-0.frg" #rx"Expected a sig")
;    (list "froglet-uni-1.frg" #rx"Expected a sig")
;    (list "froglet-uni-2.frg" #rx"is not a value") ;; TODO better block checking
;    (list "froglet-uni-3.frg" #rx"Expected a sig")
;    (list "froglet-uni-4.frg" #rx"Expected a sig") ;; TODO reachable, double-check

    (list "example_impossible.frg" #rx"impossible")
    (list "excluded-extender-value.frg" #rx"not a subset")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_theorem.frg" #rx"failed.")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_unsat.frg" #rx"Failed test")
    (list "failed_sat.frg" #rx"Failed test") 
    (list "properties_undirected_tree_underconstraint_error.frg" #rx"Assertion isUndirected is sufficient for isUndirectedTree failed.")
    (list "properties_undirected_tree_overconstraint_error.frg" #rx"Assertion TreeWithEdges is necessary for isUndirectedTree failed.")
    (list "formula_comprehension_cardinality.frg" #rx"expected to be given")
    (list "formula_comprehension_multiplicity.frg" #rx"expected to be given")
    (list "hello.frg" #rx"parsing error")
    (list "ill_typed_inst_columns_reversed.frg" #rx"age")
    (list "inst-undefined-bound-child-one.frg" #rx"for an ancestor of")
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
        (check-exn pred (lambda () (run-error-test test-name)))))
    (void)))

(define (run-error-test test-name)
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-directory here])
    (let* ([root-module `(file ,(path->string (build-path here test-name)))])
      (dynamic-require root-module #f)
      (dynamic-require `(submod ,root-module execs) #f))))

;; -----------------------------------------------------------------------------

(module+ main
  (unless (zero?
            (run-tests
              (test-suite "error/main.rkt"
                (test-case "error tests"
                  (run-error-tests REGISTRY)))))
    (exit 1)))


