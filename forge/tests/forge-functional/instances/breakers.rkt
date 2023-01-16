#lang forge/core

option run_sterling off
(set-option! 'verbose 1)

(define A (make-sig 'A))
(define R (make-relation 'R (list A A)))

(define linear-inst
  (make-inst (list
              (= A (+ (atom 'Aaron) (+ (atom 'Alice) (atom 'Andy))))
              (is R linear))))

(make-test #:name 'linearSAT
           #:bounds (list linear-inst)
           #:sigs (list A)
           #:relations (list R)
           #:expect 'sat)
(make-test #:name 'linearUNSAT
           #:preds (list (no R))
           #:bounds (list linear-inst)
           #:sigs (list A)
           #:relations (list R)
           #:expect 'unsat)
