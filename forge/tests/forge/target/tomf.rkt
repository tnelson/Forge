#lang forge/core 

(require (only-in rackunit check-eq? check-not-eq? check-true check-false))

;(require "close_fixed.frg")
;(define sat-gen (forge:make-model-generator (forge:get-result tomf_close_fixed) 'next))
;(define sat-soln1 (sat-gen))
;(check-true (forge:Sat? sat-soln1))
;(check-true (empty? (hash-ref (first (Sat-instances sat-soln1)) 'Node)))
;(define sat-soln2 (sat-gen))
;(check-true (forge:Sat? sat-soln2))
;(check-eq? (length (hash-ref (first (Sat-instances sat-soln2)) 'Node)) 1)

(require "far_fixed.frg")
(define sat-gen (forge:make-model-generator (forge:get-result tomf_far_fixed) 'next))
(define sat-soln1 (sat-gen))
(check-true (forge:Sat? sat-soln1))
(check-eq? (length (hash-ref (first (Sat-instances sat-soln1)) 'Node)) 4)
(define sat-soln2 (sat-gen))
(check-true (forge:Sat? sat-soln2))
(check-eq? (length (hash-ref (first (Sat-instances sat-soln2)) 'Node)) 4)
