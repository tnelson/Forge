#lang forge/core
(require "substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

;(define f-symmetric (= edges (~ edges)))
;(define f-irreflexive (no (& edges iden)))
;(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

;(check-eq? (to-string (substitute-formula f-symmetric '(all) )) )

(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(define f-and ())

(substitute-formula f-some-reaches-all '() 'x 'z)
