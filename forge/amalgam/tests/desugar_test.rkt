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
; desugar-formula test cases

; constant formulas
(define const (node/formula/constant empty-nodeinfo Int))
(@test-case
 "TEST 1 constant formulas"
 (@check-equal?
  (to-string (desugar-formula const '() udt #t))
  (to-string const)))

; multiplicity formula
; I think this one is working (-ABBY)
;(define f-some-reaches-all (one edges))
;(desugar-formula f-some-reaches-all '() udt #t)

; quantified formula

; no
; QUESTION: THESE RETURN VERY LONG THINGS
#|(define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define y (node/expr/quantifier-var empty-nodeinfo 1 'y))
(define no-test (no ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(define negated-formula  (node/formula/op/! empty-nodeinfo (list (in y (join x (^ edges))))))
(define new-quant-formula (node/formula/quantified empty-nodeinfo 'all (list [y Node]) negated-formula))
(@test-case
 "TEST NO formula curr-sign true"
 (@check-equal?
  (to-string (desugar-formula no-test '() udt #t))
  (to-string (desugar-formula new-quant-formula '() udt #t))))|#

; one
;(define one-test (one ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

; lone
;(define lone-test (lone ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

; desugar-formula-op test cases
; AND
(define and-test (and true false))
(@test-case
 "TEST AND formula curr-sign true"
 (@check-equal?
  (to-string (desugar-formula and-test '() udt #t))
  (to-string (node/formula/op/&& empty-nodeinfo (list true false)))))

(@test-case
 "TEST AND formula curr-sign false"
 (@check-equal?
  (to-string (desugar-formula and-test '() udt #f))
  (to-string (node/formula/op/|| empty-nodeinfo (list true false)))))

; OR
(define or-test (or true false))
(@test-case
 "TEST ORR formula curr-sign true"
 (@check-equal?
  (to-string (desugar-formula or-test '() udt #t))
  (to-string (node/formula/op/|| empty-nodeinfo (list true false)))))

(@test-case
 "TEST OR formula curr-sign false"
 (@check-equal?
  (to-string (desugar-formula or-test '() udt #f))
  (to-string (node/formula/op/&& empty-nodeinfo (list true false)))))

; IMPLIES
(define implies-test (implies true false))
(@test-case
 "TEST implies formula"
 (@check-equal?
  (to-string (desugar-formula implies-test '() udt #t))
  ;desugars to (not LHS) OR (RHS) 
  (to-string (node/formula/op/|| empty-nodeinfo (list true false)))))

; IN For Not Ground LeftE
(define in-test (in Node univ))
(define leftE Node)
(define rightE univ)
(define lifted-upper-bounds (lift-bounds-expr leftE '() udt))
(define desugaredAnd (node/formula/op/&& empty-nodeinfo
                                                 (map (lambda (x)
                                                        (define tupExpr (tup2Expr x udt empty-nodeinfo))
                                                        (define LHS  (node/formula/op/in empty-nodeinfo (list tupExpr leftE)))
                                                        (define RHS (node/formula/op/in empty-nodeinfo (list tupExpr rightE)))
                                                        (node/formula/op/=> empty-nodeinfo (list LHS RHS))) lifted-upper-bounds))) 
        


(@test-case
 "TEST In formula"
 (@check-equal?
  (to-string (desugar-formula in-test '() udt #t))
  (to-string (desugar-formula desugaredAnd '() udt #t))))

; IN for ground LeftE

; EQUALS
; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
(define equals-test (= Node Node))
(define LHS (node/formula/op/in empty-nodeinfo (list Node Node)))
(define RHS (node/formula/op/in empty-nodeinfo (list Node Node)))
(define desugaredEquals (node/formula/op/&& empty-nodeinfo (list LHS RHS)))
(@test-case
 "TEST EQUALS formula"
 (@check-equal?
  (to-string (desugar-formula equals-test '() udt #t))
  (to-string (desugar-formula  desugaredEquals '() udt #t))))

; NEGATION
; QUESTION: SHOULD THIS BE CALLING DESUGAR-FORMULA-OP INSTEAD?
(define negation-test (! true))
(@test-case
 "TEST NEGATION formula"
 (@check-equal?
  (to-string (desugar-formula negation-test '() udt #t))
  ;desugars to (not LHS) OR (RHS) 
  (to-string true)))

; INTEGER <
(define var-int-const-x (node/int/constant empty-nodeinfo 1))
(define var-int-const-y (node/int/constant empty-nodeinfo 2))
(define int-less (< var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-formula int-less '() udt #t)))

; INTEGER >
(define int-greater (> var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-formula int-greater '() udt #t)))

; INTEGER =
(define int-equals (= var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-formula int-equals '() udt #t)))

; relation name

; atom

; Int constant

; other expression constants

; quantified variable

; set comprehension

; desugar-expr test on no currTupIfAtomic
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-formula (+ Node Node) '() '() udt #t)))

; UNION
(define union-test (+ Node Node))
(define desugared-child (desugar-expr Node '() '(Node) udt #t))
(define desugared-with-or (node/formula/op/|| empty-nodeinfo (list desugared-child desugared-child)))
(@test-case
 "TEST UNION expression"
 (@check-equal?
  (to-string (desugar-expr union-test '() '(Node) udt #t))
  (to-string desugared-with-or)))


; SETMINUS
; (currTupIfAtomic in LHS) and (not(currTupIfAtomic in RHS))
#|(define setminus-test (- Node Node))
(define LHS (node/formula/op/in info (list currTupIfAtomicExpr (first args))))
(define RHS (node/formula/op/! info (list node/formula/op/in info (list currTupIfAtomicExpr (second args)))))
(define desugared-child (desugar-expr Node '() '(Node) udt #t))
(define deusgared-with-or (node/formula/op/&& empty-nodeinfo (list desugared-child desugared-child)))
(@test-case
 "TEST UNION expression"
 (@check-equal?
  (to-string (desugar-expr union-test '() '(Node) udt #t))
  (to-string deusgared-with-or)))|#

; INTERSECTION

; PRODUCT

; JOIN

; TRANSITIVE CLOSURE

; REFLEXIVE-TRANSITIVE CLOSURE

; TRANSPOSE

; SINGLETON

; CONSTANT INT

; sum
(define int-sum (sum edges))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-sum '() udt)))

; desugar-int-op
; int addition
(define int-add (+ 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-add '() udt #t)))

; int subtraction
(define int-sub (- 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-sub '() udt #t)))

; int division
(define int-div (/ 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-div '() udt #t)))

; int mult
(define int-mult (* 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-mult '() udt #t)))

; int sum
(define int-sum-err (sum Node))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-sum-err '() udt #t)))

; int card
(define int-card (card Node))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-card '() udt #t)))

; int mod
(define int-mod (modulo 0 5))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-mod '() udt #t)))

; int abs
; TODO: finish this case
#|(define int-abs (absolute 1))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-abs '() udt #t)))|#

; int sign-of
(define int-sign-of (sgn 1))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-int int-sign-of '() udt #t)))