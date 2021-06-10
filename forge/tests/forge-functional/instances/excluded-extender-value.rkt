#lang forge/core

(set-option! 'verbose 10)
; (set-verbosity 10)

(define A (make-sig 'A))
(define B (make-sig 'B #:extends A))
(define C (make-sig 'C #:extends B))

(define my-inst
  (make-inst (list
              (= A (+ (atom 'A1) (+ (atom 'A2)
                                    (+ (atom 'B1) (+ (atom 'B2)
                                                     (+ (atom 'C1) (atom 'C2)))))))
              (= B (+ (atom 'B1) (+ (atom 'B2) (atom 'C1)))) ; Missing C2 here
              (= C (+ (atom 'C1) (atom 'C2))))))

(make-test #:name 'my-test
           #:bounds (list my-inst)
           #:scope (list (list A 6))
           #:sigs (list A B C)
           #:expect 'unsat)
