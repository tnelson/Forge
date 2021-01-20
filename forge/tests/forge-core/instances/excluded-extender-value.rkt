#lang forge/core

(set-option! 'verbose 0)
; (set-verbosity 10)

(sig A)
(sig B #:extends A)
(sig C #:extends B)

(inst my-inst
    (= C (+ C1 C2))
    (= B (+ B1 (+ B2 C1))) ; Missing C2 here
    (= A (+ A1 (+ A2 (+ B1 (+ B2 (+ C1 C2)))))))

(test my-test
     #:bounds [my-inst]
     #:scope ([A 6])
     #:expect unsat)
