#lang racket/base

(require syntax/parse/define)

(require (except-in forge/sigs test example))
(provide (all-from-out forge/sigs)
         test example
         (struct-out test-report))

(struct test-report (name passed?) #:prefab)

(define-syntax-rule (test name args ... #:expect expected)
  (cond 
    [(member 'expected '(sat unsat))
     (run name args ...)
     (define first-instance (tree:get-value (forge:Run-result name)))
     (define ret (test-report 'name (equal? (if (Sat? first-instance) 'sat 'unsat) 'expected)))
     (forge:close-run name)
     ret]

    [(equal? 'expected 'theorem)
     (check name args ...)
     (define first-instance (tree:get-value (forge:Run-result name)))
     (define ret (test-report 'name (Unsat? first-instance)))
     (forge:close-run name)
     ret]

    [else (raise (format "Illegal argument to test. Received ~a, expected sat, unsat, or theorem."
                         'expected))]))

(define-simple-macro (example name:id pred bounds ...)
  (test name #:preds [pred]
             #:bounds [bounds ...]
             #:expect sat))
