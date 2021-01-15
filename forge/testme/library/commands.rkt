#lang racket

(require syntax/parse/define)

(require (except-in forge/sigs test example))
(provide (all-from-out forge/sigs)
         test example
         (struct-out test-report))

(struct test-report (name passed?) #:transparent)

(define-syntax-rule (test name args ... #:expect expected)
  (cond 
    [(member 'expected '(sat unsat))
     (let ()
       (run name args ...)
       (define first-instance (stream-first (forge:Run-result name)))
       (test-report 'name (equal? (car first-instance) 'expected)))]

    [(equal? 'expected 'theorem)
     (check name args ...)
     (define first-instance (stream-first (forge:Run-result name)))
     (test-report 'name (equal? (car first-instance) 'unsat))]

    [else (raise (format "Illegal argument to test. Received ~a, expected sat, unsat, or theorem."
                         'expected))]))

(define-simple-macro (example name:id pred bounds ...)
  (test name #:preds [pred]
             #:bounds [bounds ...]
             #:expect sat))