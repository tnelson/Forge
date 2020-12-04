#lang forge/core

(require "../desugar/desugar.rkt")
(require "forge_ex.rkt")
(require "test_helpers.rkt")
(require "../desugar/desugar_helpers.rkt")
(require "../lift-bounds/lift-bounds.rkt")
(require (prefix-in @ rackunit))

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])


; constant formulas
(define const (node/formula/constant empty-nodeinfo Int))
(@test-case
 "TEST 1 constant formulas"
 (@check-equal?
  (toString (desugarFormula const '() udt #t))
  (toString const)))

; multiplicity formula
; I think this one is working (-ABBY)
;(define fSomeReachesAll (one edges))
;(desugarFormula fSomeReachesAll '() udt #t)

; quantified formula

; no
; QUESTION: THESE RETURN VERY LONG THINGS
#|(define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define y (node/expr/quantifier-var empty-nodeinfo 1 'y))
(define noTest (no ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(define negatedFormula  (node/formula/op/! empty-nodeinfo (list (in y (join x (^ edges))))))
(define newQuantFormula (node/formula/quantified empty-nodeinfo 'all (list [y Node]) negatedFormula))
(@test-case
 "TEST NO formula currSign true"
 (@check-equal?
  (toString (desugarFormula noTest '() udt #t))
  (toString (desugarFormula newQuantFormula '() udt #t))))|#

; one
;(define oneTest (one ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

; lone
;(define loneTest (lone ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))


; AND
(define andTest (and true false))
(@test-case
 "TEST AND formula currSign true"
 (@check-equal?
  (toString (desugarFormula andTest '() udt #t))
  (toString (node/formula/op/&& empty-nodeinfo (list true false)))))

(@test-case
 "TEST AND formula currSign false"
 (@check-equal?
  (toString (desugarFormula andTest '() udt #f))
  (toString (node/formula/op/|| empty-nodeinfo (list true false)))))

; OR
(define orTest (or true false))
(@test-case
 "TEST ORR formula currSign true"
 (@check-equal?
  (toString (desugarFormula orTest '() udt #t))
  (toString (node/formula/op/|| empty-nodeinfo (list true false)))))

(@test-case
 "TEST OR formula currSign false"
 (@check-equal?
  (toString (desugarFormula orTest '() udt #f))
  (toString (node/formula/op/&& empty-nodeinfo (list true false)))))

; IMPLIES
(define impliesTest (implies true false))
(@test-case
 "TEST implies formula"
 (@check-equal?
  (toString (desugarFormula impliesTest '() udt #t))
  ;desugars to (not LHS) OR (RHS) 
  (toString (node/formula/op/|| empty-nodeinfo (list true false)))))

; IN For Not Ground LeftE
(define inTest (in Node univ))
(define leftE Node)
(define rightE univ)
(define liftedUpperBounds (liftBoundsExpr leftE '() udt))
(define desugaredAnd (node/formula/op/&& empty-nodeinfo
                                         (map (lambda (x)
                                                (define tupExpr (tup2Expr x udt empty-nodeinfo))
                                                (define LHS  (node/formula/op/in empty-nodeinfo (list tupExpr leftE)))
                                                (define RHS (node/formula/op/in empty-nodeinfo (list tupExpr rightE)))
                                                (node/formula/op/=> empty-nodeinfo (list LHS RHS))) liftedUpperBounds))) 
        


(@test-case
 "TEST In formula"
 (@check-equal?
  (toString (desugarFormula inTest '() udt #t))
  (toString (desugarFormula desugaredAnd '() udt #t))))

; IN for ground LeftE

; EQUALS
; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
(define equalsTest (= Node Node))
(define LHS (node/formula/op/in empty-nodeinfo (list Node Node)))
(define RHS (node/formula/op/in empty-nodeinfo (list Node Node)))
(define desugaredEquals (node/formula/op/&& empty-nodeinfo (list LHS RHS)))
(@test-case
 "TEST EQUALS formula"
 (@check-equal?
  (toString (desugarFormula equalsTest '() udt #t))
  (toString (desugarFormula  desugaredEquals '() udt #t))))

; NEGATION
; QUESTION: SHOULD THIS BE CALLING DESUGARFORMULAOP INSTEAD?
(define negationTest (! true))
(@test-case
 "TEST NEGATION formula"
 (@check-equal?
  (toString (desugarFormula negationTest '() udt #t))
  ;desugars to (not LHS) OR (RHS) 
  (toString true)))

; INTEGER <
(define varIntConstX (node/int/constant empty-nodeinfo 1))
(define varIntConstY  (node/int/constant empty-nodeinfo 2))
(define intLess (< varIntConstX varIntConstY ))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarFormula intLess '() udt #t)))

; INTEGER >
(define intGreater (> varIntConstX varIntConstY ))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarFormula intGreater '() udt #t)))

; INTEGER =
(define intEquals (= varIntConstX varIntConstY ))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarFormula intEquals '() udt #t)))

; relation name

; atom

; Int constant

; other expression constants

; quantified variable

; set comprehension

; desugarExpr test on no currTupIfAtomic
(@check-exn
 exn:fail?
 (lambda () 
   (desugarFormula (+ Node Node) '() '() udt #t)))

; UNION
(define unionTest (+ Node Node))
(define desugaredChild (desugarExpr Node '() '(Node) udt #t))
(define desugaredWithOr (node/formula/op/|| empty-nodeinfo (list desugaredChild desugaredChild)))
(@test-case
 "TEST UNION expression"
 (@check-equal?
  (toString (desugarExpr unionTest '() '(Node) udt #t))
  (toString desugaredWithOr)))


; SETMINUS
; (currTupIfAtomic in LHS) and (not(currTupIfAtomic in RHS))
#|(define setMinusTest (- Node Node))
(define LHSSetMinus (node/formula/op/in info (list currTupIfAtomicExpr (first args))))
(define RHSSetMinus (node/formula/op/! info (list node/formula/op/in info (list currTupIfAtomicExpr (second args)))))
(define desugaredSetMinus (node/formula/op/&& info (list LHSSetMinus RHSSetMinus)))
(@test-case
 "TEST UNION expression"
 (@check-equal?
  (toString (desugarExpr setMinusTest '() '(Node) udt #t))
  (toString (desugarFormula desugaredSetMinus quantVars runContext currSign))))|#

; INTERSECTION

; PRODUCT

; JOIN

; TRANSITIVE CLOSURE

; REFLEXIVE-TRANSITIVE CLOSURE

; TRANSPOSE

; SINGLETON

;;;;;;;;;;;;;;;;;; DESUGAR INT ;;;;;;;;;;;;;;;;;;;;

; CONSTANT INT

; sum
(define intSum (sum edges))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intSum '() udt)))

; desugarIntOp
; int addition
(define intAdd (+ 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intAdd '() udt #t)))

; int subtraction
(define intSub (- 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intSub '() udt #t)))

; int division
(define intDiv (/ 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intDiv '() udt #t)))

; int mult
(define intMult (* 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intMult '() udt #t)))

; int sum
(define intSumErr (sum Node))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intSumErr '() udt #t)))

; int card
(define intCard (card Node))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intCard '() udt #t)))

; int mod
(define intMod (modulo 0 5))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intMod '() udt #t)))

; int abs
; TODO: finish this case
#|(define intAbs (absolute 1))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intAbs '() udt #t)))|#

; int sign-of
(define intSignOf (sgn 1))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intSignOf '() udt #t)))