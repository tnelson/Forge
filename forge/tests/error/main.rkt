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
    (list "bsl-int-minus.frg" #rx"Froglet")
    (list "bsl-intersect-ast.frg" #rx"recognize") ;; TODO why???!
    (list "bsl-join-right.frg" #rx"not a singleton")
    (list "bsl-join-right2.frg" #rx"not a singleton")
    (list "bsl-join-right3.frg" #rx"not a singleton")
    (list "bsl-join-right4.frg" #rx"not a singleton")
    (list "bsl-join-right5.frg" #rx"bsl")
    (list "bsl-join-right6.frg" #rx"not a singleton")
    (list "bsl-join.frg" #rx"not a singleton")
    (list "bsl-join2.frg" #rx"not a singleton")
    (list "bsl-join3.frg" #rx"not a singleton")
    (list "bsl-minus-ast.frg" #rx"recognize")
    (list "bsl-plus-ast.frg" #rx"recognize")
    (list "bsl-reachable.frg" #rx"field")
    (list "bsl-reachable2.frg" #rx"First")
    (list "bsl-set-singleton-equal.frg" #rx"singleton")
    (list "bsl-set.frg" #rx"Froglet")
    (list "bsl-star-ast.frg" #rx"recognize")
    (list "bsl-transitive-closure-ast.frg" #rx"recognize")
    (list "bsl-transpose-ast.frg" #rx"recognize")
    (list "bsl-transpose.frg" #rx"recognize")

    (list "example_impossible.frg" #rx"impossible")
    (list "excluded-extender-value.frg" #rx"not a subset")
    (list "failed_sat.frg" #rx"Failed test")
    (list "failed_theorem.frg" #rx"failed.")
    (list "failed_unsat.frg" #rx"Failed test")
    (list "properties_undirected_tree_underconstraint_error.frg" #rx"isUndirectedTree implies TreeWithEdges failed. Found instance")
    (list "properties_undirected_tree_overconstraint_error.frg" #rx"isUndirected implies isUndirectedTree failed. Found instance")
    (list "formula_comprehension_cardinality.frg" #rx"expected to be given")
    (list "formula_comprehension_multiplicity.frg" #rx"expected to be given")
    (list "hello.frg" #rx"parsing error")
    (list "ill_typed_inst_columns_reversed.frg" #rx"age")
    (list "inst-undefined-bound-child-one.frg" #rx"for an ancestor of")
  ))

; froglet-arrow-ast.frg
; froglet-arrow.frg
; froglet-int-minus.frg
; froglet-intersect-ast.frg
; froglet-join-right.frg
; froglet-join-right2.frg
; froglet-join-right3.frg
; froglet-join-right4.frg
; froglet-join-right5.frg
; froglet-join-right6.frg
; froglet-join.frg
; froglet-join2.frg
; froglet-join3.frg
; froglet-minus-ast.frg
; froglet-plus-ast.frg
; froglet-reachable.frg
; froglet-reachable2.frg
; froglet-set-singleton-equal.frg
; froglet-set.frg
; froglet-star-ast.frg
; froglet-transitive-closure-ast.frg
; froglet-transpose-ast.frg
; froglet-transpose.frg


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


