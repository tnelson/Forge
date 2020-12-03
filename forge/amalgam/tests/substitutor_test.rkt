#lang forge/core
(require "../substitutor/substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require debug/repl)
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(define varz (node/expr/quantifier-var empty-nodeinfo 1 'z))
(define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define varzArity2 (node/expr/quantifier-var empty-nodeinfo 2 'z))

; Checking substitution in base cases
(@test-case
 "TEST substituteFormula in base cases"
(@check-equal?
 (toString (substituteFormula (substituteFormula (no (& edges iden)) '() iden univ) '() univ iden))
 (toString (no (& edges iden)))))

; Checking substitution in replacing relation names base case
(@test-case
 "TEST substituteFormula in relation base case"
(@check-equal?
 (toString (substituteFormula (= edges (~ edges)) '() edges varzArity2))
 (toString (= varzArity2 (~ varzArity2)))))

; Checking substitution in union case
(@test-case
 "TEST substituteExpr in union case"
(@check-equal?
 (toString (substituteExpr (+ edges iden) '() edges iden))
 (toString (+ iden iden))))

; Checking substitution in set minus case
(@test-case
 "TEST substituteExpr in set minus case"
(@check-equal?
 (toString (substituteExpr (- iden (+ edges iden)) '() edges iden))
 (toString (- iden (+ iden iden)))))

; Checking substitution in intersection case
(@test-case
 "TEST substituteExpr in intersection case"
(@check-equal?
 (toString (substituteExpr (& edges (- edges (+ edges edges))) '() edges iden))
 (toString (& iden (- iden (+ iden iden))))))

; Checking substitution in product case
(@test-case
 "TEST substituteExpr in product case"
(@check-equal?
 (toString (substituteExpr (-> edges (-> edges Node)) '() Node univ))
 (toString (-> edges (-> edges univ)))))

; Checking substitution in join case
(@test-case
 "TEST substituteExpr in join case"
(@check-equal?
 (toString (substituteExpr (join edges iden) '() iden edges))
 (toString (join edges edges))))

; Checking substitution in transitive closure case
(@test-case
 "TEST substituteExpr in transitive closure case"
(@check-equal?
 (toString (substituteExpr (^ edges) '() edges varzArity2))
 (toString (^ varzArity2))))

; Checking substitution in reflexive-transitive closure case
(@test-case
 "TEST substituteExpr in reflexive-transitive closure case"
(@check-equal?
 (toString (substituteExpr (* edges) '() edges varzArity2))
 (toString (* varzArity2))))

; Checking substitution in transpose case
(@test-case
 "TEST substituteExpr in transpose case"
(@check-equal?
 (toString (substituteExpr (~ edges) '() edges varzArity2))
 (toString (~ varzArity2))))

(define fSomeReachesAll (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))


; Checking substitution in join case (more complicated)
(@test-case
 "TEST substituteFormula in complicated join case"
(@check-equal?
 (toString (substituteFormula fSomeReachesAll '() edges varzArity2))
 (toString (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2))))))))

; Checking substitution in quantifier-var case. Should not throw variable shadowing error. 
(@test-case
 "TEST substituteFormula in quantifier-var case"
(define freeXReachesAll (all ([y Node]) (in y (join varx (^ edges)))))
(@check-equal?
 (toString (substituteFormula freeXReachesAll '() varx varz))
 (toString (all ([y Node]) (in y (join varz (^ edges)))))))

(define fSomeReachesAllComplicated (and
                                        (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                        (all ([y Node]) (in y (join varx (^ edges))))))
; Checking substitution in and case
(@test-case
 "TEST substituteFormula in 'and' case"
(substituteFormula fSomeReachesAllComplicated '() edges varz)
(@check-equal?
 (toString (substituteFormula fSomeReachesAllComplicated '() edges varzArity2))
 (toString (and
             (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
             (all ([y Node]) (in y (join varx (^ varzArity2))))))))

; Checking substitution in quantifier-var case. Should throw variable shadowing error.
(@check-exn
 exn:fail?
 (lambda () 
   (substituteFormula fSomeReachesAll '() varx varz)))

(define qvx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define fSetComprehension (node/expr/comprehension empty-nodeinfo 1
                                  (list (cons qvx Node))
                                  (in qvx (join edges qvx))))

; set comprehension
(@test-case
 "TEST substituteExpr in set comprehension case"

(@check-equal?
 (toString (substituteExpr fSetComprehension '() edges varzArity2))
 (toString (node/expr/comprehension empty-nodeinfo 1
                                  (list (cons qvx Node))
                                  (in qvx (join varzArity2 qvx))))))

; set comprehension shadow error
(@check-exn
 exn:fail?
 (lambda () 
   (substituteExpr fSetComprehension '() qvx varzArity2)))


; formula constant
(define varConstX (node/formula/constant empty-nodeinfo Int))
(define varConstY (node/formula/constant empty-nodeinfo Int))

(@test-case
 "TEST substituteFormula in formula constant case 1" 
(@check-equal?
 (toString (substituteFormula varConstX '() varConstX varConstY))
 (toString varConstY)))
             
(@test-case
 "TEST substituteFormula in formula constant case 2" 
(@check-equal?
 (toString (substituteFormula varConstX '() varConstX varConstX))
 (toString varConstX)))

; Checking substitution in OR case
(@test-case
 "TEST substituteFormula in 'OR' case " 
(define fSomeReachesAllComplicatedOr (or
                                           (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                           (all ([y Node]) (in y (join varx (^ edges))))))

(@check-equal?
 (toString (substituteFormula fSomeReachesAllComplicatedOr '() edges varzArity2))
 (toString (or
             (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
             (all ([y Node]) (in y (join varx (^ varzArity2))))))))


; formula implies
(@test-case
 "TEST substituteFormula in implies case " 
(define fSomeReachesAllComplicatedImplies (implies
                                                (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                                (all ([y Node]) (in y (join varx (^ edges))))))
(@check-equal?
 (toString (substituteFormula fSomeReachesAllComplicatedImplies '() edges varzArity2))
 (toString (implies
             (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
             (all ([y Node]) (in y (join varx (^ varzArity2))))))))


; formula in
(define fSomeReachesAllSimpleIn (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(define fSomeReachesAllComplexIn (and
                                     (or (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                         (no ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
                                     (all ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))))

(@test-case
 "TEST substituteFormula in 'IN' case " 
(@check-equal?
 (toString (substituteFormula fSomeReachesAllSimpleIn '() edges varzArity2))
 (toString (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2))))))))

(@test-case
 "TEST substituteFormula in complex in case " 
(@check-equal?
 (toString (substituteFormula fSomeReachesAllComplexIn '() edges varzArity2))
 (toString (and
             (or (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
                 (no ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2))))))
             (all ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))))))

; formula negation
(@test-case
 "TEST substituteFormula in negation case " 
(define fSomeReachesAllSimpleNegation (not (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))))
(@check-equal?
 (toString (substituteFormula fSomeReachesAllSimpleNegation '() edges varzArity2))
 (toString (not (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))))))

; formula integer >
(define varIntConstX (node/int/constant empty-nodeinfo 1))
(define varIntConstY (node/int/constant empty-nodeinfo 2))
(define fIntGreater (> varIntConstX varIntConstY))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt fIntGreater '() varIntConstX varIntConstY)))

; formula integer <
(define fIntLess (< varIntConstX varIntConstY))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt fIntLess '() varIntConstX varIntConstY)))

; formula integer =
(define fIntEqual (= varIntConstX varIntConstY))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt fIntEqual '() varIntConstX varIntConstY)))

; operator
(define fIntPlus (node/int/op/add empty-nodeinfo (list varIntConstX varIntConstY)))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt fIntPlus '() varIntConstX varIntConstY)))

; cardinality
(@test-case
 "TEST substituteInt in cardinality case " 
(define fCardinality (node/int/op/card empty-nodeinfo (list Node)))
(@check-equal?
 (toString (substituteInt fCardinality '() Node edges))
 (toString (node/int/op/card empty-nodeinfo (list edges)))))

; singleton
(@test-case
 "TEST substituteExpr in singleton case " 
(@check-equal?
 (toString (substituteExpr (sing varIntConstX) '() varIntConstX varIntConstY))
 (toString (sing varIntConstY))))

; "sum" quantifier case 
(define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define fSum (node/int/sum-quant empty-nodeinfo
                                  (list (cons x Node))
                                  (node/int/op/card empty-nodeinfo (list (join edges x)))))

(@test-case
 "TEST substituteInt in sum quantifier case " 
(@check-equal?
 (toString (substituteInt fSum '() edges varzArity2))
 (toString (node/int/sum-quant empty-nodeinfo
                                  (list (cons x Node))
                                  (node/int/op/card empty-nodeinfo (list (join varzArity2 x)))))))

; "sum" quantifier case error shadowing 
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt fSum '() x x)))

; substitute-int-op
; int addition
(define intAdd (+ 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intAdd '() udt #t)))

; int subtraction
(define intSub (- 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intSub '() udt #t)))

; int division
(define intDiv (/ 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intDiv '() udt #t)))

; int mult
(define intMult (* 1 2))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intMult '() udt #t)))

; int sum
(define intSumErr (sum Node))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intSumErr '() udt #t)))

; int mod
(define intMod (modulo 0 5))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intMod '() udt #t)))

; int abs
; TODO: finish this case
#|(define int-abs (absolute 1))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt int-abs '() udt #t)))|#

; int sign-of
(define intSignOf (sgn 1))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt intSignOf '() udt #t)))


