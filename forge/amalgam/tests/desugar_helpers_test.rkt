#lang forge/core
(require "../desugar/desugar_helpers.rkt")
(require "../desugar/desugar.rkt")
(require "../substitutor/substitutor.rkt")
(require "forge_ex.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))


(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(run-tests (@test-suite 
  " Desugar helpers tests"
  (lambda () (display "Starting tests for Desugar helpers"))
  (lambda () (display "All tests for Desugar helpers passed!"))

  ; product-helper
  (@test-case
   "TEST product-helper on valid input same arity"
     (define currTupIfAtomic (list 'Node0))
  (define LHS univ)
  (define RHS Node)
  (define rightTupleContext (projectTupleRange currTupIfAtomic (- (node/expr-arity LHS) 1) (node/expr-arity RHS)))
  (define leftTupleContext (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
  (define formulas (list
                    (node/formula/op/in empty-nodeinfo (list (tup2Expr leftTupleContext udt empty-nodeinfo) LHS))
                    (node/formula/op/in empty-nodeinfo (list (tup2Expr rightTupleContext udt empty-nodeinfo) RHS))))
   (@check-equal?
    (to-string (product-helper (list edges Node univ) (list Node edges) currTupIfAtomic empty-nodeinfo udt))
    (to-string formulas))))


(define currTupIfAtomic_different_arity (list 'Node0 'Node1))
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
(@test-case
 "TEST product-helper on valid input different arity"
 (@check-equal?
  (to-string (product-helper (list Node edges) (list Node edges) currTupIfAtomic_different_arity empty-nodeinfo udt))
  (to-string formulas_different_arity)))

; join-helper


; tup2Expr
(define tuple (list 'Node0 'Node1 'Node2))

(@test-case
 "TEST tup2ExprValid"
 (@check-equal?
  (to-string (tup2Expr tuple udt empty-nodeinfo))
  (to-string (node/expr/op/-> empty-nodeinfo 3 (list
                                                (node/expr/atom empty-nodeinfo 1 'Node0)
                                                (node/expr/atom empty-nodeinfo 1 'Node1)
                                                (node/expr/atom empty-nodeinfo 1 'Node2))))))
; empty list
(@check-exn
 exn:fail?
 (lambda () 
   (tup2Expr '() udt empty-nodeinfo)))

; not a list
(@check-exn
 exn:fail?
 (lambda () 
   (tup2Expr 'Node0 udt empty-nodeinfo)))

; element that is a list
(@check-exn
 exn:fail?
 (lambda () 
   (tup2Expr (list (list 'Node0)) udt empty-nodeinfo)))

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
; not list
(@check-exn
 exn:fail?
 (lambda () 
   (mustHaveTupleContext 'Node0 edges)))


; length of tup is 0
(@check-exn
 exn:fail?
 (lambda () 
   (mustHaveTupleContext '() edges)))

; first thing in tuple is a list
(@check-exn
 exn:fail?
 (lambda () 
   (mustHaveTupleContext (list (list 'Node0)) edges)))


; isGroundProduct
; not an expr or int constant error
(@check-exn
 exn:fail?
 (lambda () 
   (isGroundProduct (node/formula/op/&& empty-nodeinfo (list 1 2)))))

; quantifier-var
(@check-exn
 exn:fail?
 (lambda () 
   (isGroundProduct (node/expr/quantifier-var empty-nodeinfo 1 (gensym "m2q")))))

; return false
(define product-expr (node/expr/op/-> empty-nodeinfo 2 (list Node Node)))
(@test-case
 "TEST isGroundProduct on valid product-expr"
 (@check-equal?
  (to-string (isGroundProduct product-expr))
  (to-string #f)))

; product case, should return true
(define quantifier-expr (node/expr/op/-> empty-nodeinfo 1 (list (node/expr/atom empty-nodeinfo 1 'Node0))))
(@test-case
 "TEST isGroundProduct quantifier case"
 (@check-equal?
  (to-string (isGroundProduct quantifier-expr))
  (to-string #t)))

; node int constant 
(@test-case
 "TEST isGroundProduct on valid constant"
 (@check-equal?
  (to-string (isGroundProduct (node/int/constant empty-nodeinfo 1)))
  (to-string #t)))

; atom
(@test-case
 "TEST transposeTup on valid tuple"
 (@check-equal?
  (to-string (transposeTup tup-arity-2))
  (to-string (list 2 1))))

; getColumnRight
; error
(@check-exn
 exn:fail?
 (lambda () 
   (getColumnRight '())))

; valid arity more than 1 (hits recursive call and base case)
(@test-case
 "TEST getColumnRight on valid"
 (@check-equal?
  (to-string (getColumnRight edges))
  (to-string (join univ edges))))

; getColumnLeft
; error
(@check-exn
 exn:fail?
 (lambda () 
   (getColumnLeft '())))

; valid arity more than 1 (hits recursive call and base case)
(@test-case
 "TEST getColumnLeft on valid"
 (@check-equal?
  (to-string (getColumnLeft edges))
  (to-string (join edges univ))))

; createNewQuantifier
(define form (in edges edges))

; error decl does not contain both things 
(@check-exn
 exn:fail?
 (lambda () 
   (createNewQuantifier (cons 1 '()) '() form udt empty-nodeinfo 'some '())))

; error desugaring unsupported
(@check-exn
 exn:fail?
 (lambda () 
   (createNewQuantifier (cons 1 2) '() form udt empty-nodeinfo 'no '())))

; some case
(define node-bound (list (list 'Node0) (list 'Node1) (list 'Node2) (list 'Node3) (list 'Node4) (list 'Node5) (list 'Node6)))
(define some-formula (some ([x Node]) (in x Node)))
(define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define subformulas (map (lambda (tup)
                               (substitute-formula (in varx Node) (list varx) varx (tup2Expr tup udt empty-nodeinfo)))
                             node-bound))

(@test-case
 "TEST createNewQuantifier on 'some' quantifier with just one decl"
 (@check-equal?
  (to-string (createNewQuantifier (cons varx Node) (list varx) (in varx Node) udt empty-nodeinfo 'some some-formula))
  (to-string (node/formula/op/|| empty-nodeinfo subformulas)))) 

; all case
(define all-formula (all ([x Node]) (in x Node)))
(@test-case
 "TEST createNewQuantifier on 'all' quantifier with just one decl"
 (@check-equal?
  (to-string (createNewQuantifier (cons varx Node) (list varx) (in varx Node) udt empty-nodeinfo 'all all-formula))
  (to-string (node/formula/op/&& empty-nodeinfo subformulas))))

; transitive-closure-helper
(@test-case
 "TEST transitive-closure-helper"
 (@check-equal?
  (to-string (transitive-closure-helper edges '() 2 0 empty-nodeinfo))
  (to-string (list edges (join edges edges)))))