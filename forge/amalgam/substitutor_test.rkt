#lang forge/core
(require "substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

;we call substitute-formula from set case, set comprehension, and quantifier case
(define f-set-comprehension (all ([y Node]) (in y edges)))

;(define f-symmetric (= edges (~ edges)))
;(define f-irreflexive (no (& edges iden)))
;(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

(define (call-substitute-formula formula)
  (substitute-formula formula quantvars target value))


(check-eq? (to-string (substitute-formula f-symmetric '(all) )) )
