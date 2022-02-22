#lang racket/base

;; Error tester
;;
;; How to use:
;;  1. Make a new file* that gives an error at compile- or run-time
;;  2. Add the file's name to the REGISTRY below
;;  3. Also add a regular expression or predicate** to the REGISTRY
;;  4. Run this file (main.rkt) to test
;;
;; * 2021-11-04: Don't use a `.rkt` extension. Pick anything else --- so the toplevel
;;    `run-tests.sh` script doesn't try to run your new file.
;; ** Works the same way as RackUnit's check-exn <https://docs.racket-lang.org/rackunit/api.html>A

(require
  rackunit
  (only-in rackunit/text-ui run-tests)
  racket/runtime-path)

(define-runtime-path here ".")

;; -----------------------------------------------------------------------------

(define REGISTRY
  (list
    (list "hello.frg" #rx"parsing error")
    (list "arrow.frg" #rx"Direct use of ->")
    (list "join.frg" #rx"singleton")
    (list "join2.frg" #rx"singleton")
    (list "join3.frg" #rx"singleton")
    (list "join-right.frg" #rx"singleton")
    (list "join-right2.frg" #rx"singleton")
    (list "join-right3.frg" #rx"singleton")
    (list "join-right4.frg" #rx"singleton")
    (list "reachable.frg" #rx"field")
    (list "reachable2.frg" #rx"First")
    (list "set.frg" #rx"Froglet")
    (list "int-minus.frg" #rx"Froglet")
    (list "set-singleton-equal.frg" #rx"singleton")
    (list "formula_comprehension_multiplicity.frg" #rx"expected to be given")
    (list "formula_comprehension_cardinality.frg" #rx"expected to be given")
    (list "inst-undefined-bound-child-one.frg" #rx"for an ancestor of")
    (list "ill_typed_inst_columns_reversed.frg" #rx"age")
    (list "excluded-extender-value.frg" #rx"not a subset")
    (list "example_impossible.frg" #rx"impossible")
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


