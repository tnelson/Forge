#lang forge/core

(set-option! 'verbose 0)
; (set-verbosity 10)

(sig A)
(sig B #:extends A)
(sig C #:extends B)

(inst my-inst
      (= C (+ (atom 'C1) (atom 'C2)))
      (= B (+ (atom 'B1) (+ (atom 'B2) (atom 'C1)))) ; Missing C2 here
      (= A (+ (atom 'A1) (+ (atom 'A2)
                            (+ (atom 'B1) (+ (atom 'B2)
                                             (+ (atom 'C1) (atom 'C2))))))))

(test my-test
     #:bounds [my-inst]
     #:scope ([A 6])
     #:expect unsat)
