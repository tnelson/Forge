#lang forge/core
(require "../substitutor/substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require debug/repl)
(require rackunit/text-ui)
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(define varz (node/expr/quantifier-var empty-nodeinfo 1 'z))
(define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define vary (node/expr/quantifier-var empty-nodeinfo 1 'y))

(define varzArity2 (node/expr/quantifier-var empty-nodeinfo 2 'z))
(define fSomeReachesAll (some ([x Node]) (all ([y Node])
                                              (in y (join x (^ edges))))))
(define varConstX (node/formula/constant empty-nodeinfo Int))
(define varConstY (node/formula/constant empty-nodeinfo Int))
(define qvx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define fSetComprehension (node/expr/comprehension empty-nodeinfo 1
                                                   (list (cons qvx Node))
                                                   (in qvx (join edges qvx))))
(define varIntConstX (node/int/constant empty-nodeinfo 1))
(define varIntConstY (node/int/constant empty-nodeinfo 2))

(run-tests
 (@test-suite 
  " Substitutor tests"
  (lambda () (display "Starting tests for Substitutor"))
  (lambda () (display "All tests for Substitutor passed!"))
  
  (@test-suite
   " SubstituteExpr tests"
   (lambda () (display "Starting tests for SubstituteExpr"))
   (lambda () (display "All tests for SubstituteExpr passed!"))

   ; Checking substitution in base cases
   (@test-case
    "TEST substituteFormula in base cases"
    (@check-equal?
     (substituteFormula (substituteFormula (no (& edges iden)) '()
                                                     iden iden) '() iden iden)
     (no (& edges iden))))

   ; Checking substitution in replacing relation names base case
   (@test-case
    "TEST substituteFormula in relation base case"
    (@check-equal?
     (substituteFormula (= edges (~ edges)) '() edges varzArity2)
     (= varzArity2 (~ varzArity2))))

   ; Checking substitution in join case (more complicated)
   (@test-case
    "TEST substituteFormula in complicated join case"
    (@check-equal?
     (substituteFormula fSomeReachesAll '() edges varzArity2)
     (some ([x Node]) (all ([y Node])
                                     (in y (join x (^ varzArity2)))))))

   ; Checking substitution in quantifier-var case. Should not throw
   ; variable shadowing error. 
   (@test-case
    "TEST substituteFormula in quantifier-var case"
    (define freeXReachesAll (all ([y Node]) (in y (join varx (^ edges)))))
    (@check-equal?
     (substituteFormula freeXReachesAll '() varx varz)
     (all ([y Node]) (in y (join varz (^ edges))))))

   ; Checking substitution in and case
   (@test-case
    "TEST substituteFormula in 'and' case"
    (define fSomeReachesAllComplicated (and
                                        (some ([x Node])
                                              (all ([y Node])
                                                   (in y (join x (^ edges)))))
                                        (all ([y Node])
                                             (in y (join varx (^ edges))))))
    (substituteFormula fSomeReachesAllComplicated '() edges varz)
    (@check-equal?
     (substituteFormula fSomeReachesAllComplicated
                                  '() edges varzArity2)
     (and
                (some ([x Node]) (all ([y Node])
                                      (in y (join x (^ varzArity2)))))
                (all ([y Node]) (in y (join varx (^ varzArity2)))))))

   (@test-case
    "TEST substituteFormula in quantifier-var variable shadowing error." 
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteFormula fSomeReachesAll '() varx varz))))

   #|(@test-case
    "TEST substituteFormula in formula constant case 1" 
    (@check-equal?
     (toString (substituteFormula varConstX '() varConstX varConstY))
     (toString varConstY)))|#
             
   #|(@test-case
    "TEST substituteFormula in formula constant case 2" 
    (@check-equal?
     (toString (substituteFormula varConstX '() varConstX varConstX))
     (toString varConstX)))|#

   ; Checking substitution in OR case
   (@test-case
    "TEST substituteFormula in 'OR' case " 
    (define fSomeReachesAllComplicatedOr (or
                                          (some ([x Node])
                                                (all ([y Node])
                                                     (in y (join x (^ edges)))))
                                          (all ([y Node])
                                               (in y (join varx (^ edges))))))

    (@check-equal?
     (substituteFormula fSomeReachesAllComplicatedOr
                                  '() edges varzArity2)
     (or
                (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
                (all ([y Node]) (in y (join varx (^ varzArity2)))))))


   ; formula implies
   (@test-case
    "TEST substituteFormula in implies case " 
    (define fSomeReachesAllComplicatedImplies
      (implies (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
               (all ([y Node]) (in y (join varx (^ edges))))))
    (@check-equal?
     (substituteFormula fSomeReachesAllComplicatedImplies
                                  '() edges varzArity2)
     (implies
                (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
                (all ([y Node]) (in y (join varx (^ varzArity2)))))))


   ; formula in
   (@test-case
    "TEST substituteFormula in 'IN' case "
    (define fSomeReachesAllSimpleIn (some ([x Node])
                                          (all ([y Node])
                                               (in y (join x (^ edges))))))
    (@check-equal?
     (substituteFormula fSomeReachesAllSimpleIn
                                  '() edges varzArity2)
     (some ([x Node]) (all ([y Node])
                                     (in y (join x (^ varzArity2)))))))

   ; formula in
   (@test-case
    "TEST substituteFormula in 'IN' case "
    (define fSomeReachesAllSimpleIn (some ([x Node])
                                          (all ([y Node])
                                               (in y (join x (^ edges))))))
    (@check-equal?
     (substituteFormula (in vary (join varx (^ edges)))
                                  '() qvx varz)
     (in vary (join varz (^ edges)))))

   (@test-case
    "TEST substituteFormula in complex in case "
    (define fSomeReachesAllComplexIn
      (and
       (or (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
           (no ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
       (all ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))))
    (@check-equal?
     (substituteFormula fSomeReachesAllComplexIn '() edges varzArity2)
      (and
       (or (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))
           (no ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2))))))
       (all ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2))))))))

   ; formula negation
   (@test-case
    "TEST substituteFormula in negation case " 
    (define fSomeReachesAllSimpleNegation
      (not (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))))
    (@check-equal?
      (substituteFormula fSomeReachesAllSimpleNegation '() edges varzArity2)
      (not (some ([x Node]) (all ([y Node]) (in y (join x (^ varzArity2)))))))))

  (@test-suite
   " SubstituteFormula tests"
   (lambda () (display "Starting tests for substituteFormula"))
   (lambda () (display "All tests for substituteFormula passed!"))
   ; Checking substitution in union case
   (@test-case
    "TEST substituteExpr in union case"
    (@check-equal?
     (substituteExpr (+ edges iden) '() edges iden)
     (+ iden iden)))

   ; Checking substitution in set minus case
   (@test-case
    "TEST substituteExpr in set minus case"
    (@check-equal?
     (substituteExpr (- iden (+ edges iden)) '() edges iden)
     (- iden (+ iden iden))))

   ; Checking substitution in intersection case
   (@test-case
    "TEST substituteExpr in intersection case"
    (@check-equal?
      (substituteExpr (& edges (- edges (+ edges edges))) '() edges iden)
      (& iden (- iden (+ iden iden)))))

   ; Checking substitution in product case
   (@test-case
    "TEST substituteExpr in product case"
    (@check-equal?
     (substituteExpr (-> edges (-> edges Node)) '() Node univ)
     (-> edges (-> edges univ))))

   ; Checking substitution in join case
   (@test-case
    "TEST substituteExpr in join case"
    (@check-equal?
     (substituteExpr (join edges iden) '() iden edges)
     (join edges edges)))

   ; Checking substitution in transitive closure case
   (@test-case
    "TEST substituteExpr in transitive closure case"
    (@check-equal?
     (substituteExpr (^ edges) '() edges varzArity2)
     (^ varzArity2)))

   ; Checking substitution in reflexive-transitive closure case
   (@test-case
    "TEST substituteExpr in reflexive-transitive closure case"
    (@check-equal?
     (substituteExpr (* edges) '() edges varzArity2)
     (* varzArity2)))

   ; Checking substitution in transpose case
   (@test-case
    "TEST substituteExpr in transpose case"
    (@check-equal?
     (substituteExpr (~ edges) '() edges varzArity2)
     (~ varzArity2)))

   ; set comprehension
   (@test-case
    "TEST substituteExpr in set comprehension case"

    (@check-equal?
     (substituteExpr fSetComprehension '() edges varzArity2)
     (node/expr/comprehension empty-nodeinfo 1
                                        (list (cons qvx Node))
                                        (in qvx (join varzArity2 qvx)))))

   (@test-case
    "TEST substituteExpr set comprehension shadowing error"
    ; set comprehension shadow error
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteExpr fSetComprehension '() qvx varzArity2)))))

  (@test-suite
   " SubstituteInt tests"
   (lambda () (display "Starting tests for SubstituteInt"))
   (lambda () (display "All tests for SubstituteInt passed!"))
   ; formula integer >
   (@test-case
    "TEST substituteInt > error"
    (define fIntGreater (> varIntConstX varIntConstY))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt fIntGreater '() varIntConstX varIntConstY))))

   ; formula integer <
   (@test-case
    "TEST substituteInt < error"
    (define fIntLess (< varIntConstX varIntConstY))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt fIntLess '() varIntConstX varIntConstY))))

   ; formula integer =
   (@test-case
    "TEST substituteInt = error"
    (define fIntEqual (= varIntConstX varIntConstY))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt fIntEqual '() varIntConstX varIntConstY))))

   ; operator
   (@test-case
    "TEST substituteInt int + error"
    (define fIntPlus
      (node/int/op/add empty-nodeinfo (list varIntConstX varIntConstY)))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt fIntPlus '() varIntConstX varIntConstY))))

   ; cardinality
   (@test-case
    "TEST substituteInt in cardinality case " 
    (define fCardinality (node/int/op/card empty-nodeinfo (list Node)))
    (@check-equal?
     (substituteInt fCardinality '() Node edges)
     (node/int/op/card empty-nodeinfo (list edges))))

   ; singleton
   #|(@test-case
    "TEST substituteExpr in singleton case " 
    (@check-equal?
     (toString
      (substituteExpr (sing varIntConstX) '() varIntConstX varIntConstY))
     (toString (sing varIntConstY))))|#

   ; "sum" quantifier case 
   (@test-case
    "TEST substituteInt in sum quantifier case "
    (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
    (define fSum
      (node/int/sum-quant empty-nodeinfo (list (cons x Node))
                          (node/int/op/card empty-nodeinfo
                                            (list (join edges x)))))
    (@check-equal?
     (substituteInt fSum '() edges varzArity2)
      (node/int/sum-quant empty-nodeinfo (list (cons x Node))
                          (node/int/op/card empty-nodeinfo
                                            (list (join varzArity2 x))))))

   ; "sum" quantifier case error shadowing
   (@test-case
    "TEST substituteInt in sum quantifier case error "
    (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
    (define fSum (node/int/sum-quant empty-nodeinfo
                                     (list (cons x Node))
                                     (node/int/op/card empty-nodeinfo
                                                       (list (join edges x)))))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt fSum '() x x))))

   ; substitute-int-op
   ; int addition
   (@test-case
    "TEST substituteIntOp in add case "
    (define intAdd (+ 1 2))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intAdd '() udt #t))))

   (@test-case
    "TEST substituteIntOp in subtract case "
    ; int subtraction
    (define intSub (- 1 2))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intSub '() udt #t))))

   ; int division
   (@test-case
    "TEST substituteIntOp in divide case "
    (define intDiv (/ 1 2))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intDiv '() udt #t))))

   ; int mult
   (@test-case
    "TEST substituteIntOp in mult case "
    (define intMult (* 1 2))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intMult '() udt #t))))

   ; int sum
   (@test-case
    "TEST substituteInt in sum case "
    (define intSumErr (sum Node))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intSumErr '() udt #t))))

   ; int mod
   (@test-case
    "TEST substituteInt in mod case "
    (define intMod (modulo 0 5))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intMod '() udt #t))))

   ; int abs
   ; TODO: finish this case
   #|(define int-abs (absolute 1))
(@check-exn
 exn:fail?
 (lambda () 
   (substituteInt int-abs '() udt #t)))|#

   ; int sign-of
   (@test-case
    "TEST substituteInt in signOf case "
    (define intSignOf (sgn 1))
    (@check-exn
     exn:fail?
     (lambda () 
       (substituteInt intSignOf '() udt #t)))))))