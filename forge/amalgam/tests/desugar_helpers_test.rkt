#lang forge/core
(require "../desugar/desugar_helpers.rkt")
(require "forge_ex.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))


(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

; product-helper
(define currTupIfAtomic (list 'Node0))
(define LHS univ)
(define RHS Node)
(define rightTupleContext (projectTupleRange currTupIfAtomic (- (node/expr-arity LHS) 1) (node/expr-arity RHS)))
(define leftTupleContext (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
(define formulas (list
                  (node/formula/op/in empty-nodeinfo (list (tup2Expr leftTupleContext udt empty-nodeinfo) LHS))
                  (node/formula/op/in empty-nodeinfo (list (tup2Expr rightTupleContext udt empty-nodeinfo) RHS))))
(@test-case
 "TEST product-helper on valid input same arity"
 (@check-equal?
  (to-string (product-helper (list edges Node univ) (list Node edges) currTupIfAtomic empty-nodeinfo udt))
  (to-string formulas)))

(define currTupIfAtomic_different_arity (list 'Node0))

; arity 2
(define LHS_different_arity edges)

; arity 1
(define RHS_different_arity Node)
(define rightTupleContext_different_arity (projectTupleRange
                                           currTupIfAtomic_different_arity
                                           (- (node/expr-arity LHS_different_arity) 1)
                                           (node/expr-arity RHS_different_arity)))
(define leftTupleContext_different_arity (projectTupleRange currTupIfAtomic_different_arity 0 (node/expr-arity LHS_different_arity)))
(define formulas_different_arity (list
                                  (node/formula/op/in empty-nodeinfo (list (tup2Expr leftTupleContext_different_arity udt empty-nodeinfo) LHS_different_arity))
                                  (node/formula/op/in empty-nodeinfo (list (tup2Expr rightTupleContext_different_arity udt empty-nodeinfo) RHS_different_arity))))
#|(@test-case
 "TEST product-helper on valid input different arity"
 (@check-equal?
  (to-string (product-helper (list Node edges) (list Node edges) currTupIfAtomic_different_arity empty-nodeinfo udt))
  (to-string formulas_different_arity)))|#

; join-helper


; tup2Expr

; transposeTup
(define tup-arity-2 (list 1 2))
(define tup-arity-3 (list 1 2 3))

(@test-case
 "TEST transposeTup on valid tuple"
 (@check-equal?
  (to-string (transposeTup tup-arity-2))
  (to-string (list 2 1))))

(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup tup-arity-3)))

; mustHaveTupleContext

; isGroundProduct

; getColumnRight

; getColumnLeft

; createNewQuantifier