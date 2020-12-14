#lang forge/core

(require "../lift-bounds/lift-bounds.rkt")
(require "../lift-bounds/lift-bounds_helpers.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require rackunit/text-ui)
(require debug/repl)
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(define nodeBound (list (list 'Node0) (list 'Node1) (list 'Node2) (list 'Node3)
                        (list 'Node4) (list 'Node5) (list 'Node6)))
(define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define varExprConstant (node/expr/constant empty-nodeinfo 1 'Int))
(define varIntConstX (node/int/constant empty-nodeinfo 1))
(define varIntConstY (node/int/constant empty-nodeinfo 2))
(define intBounds (list (list -8) (list -7) (list -6) (list -5) (list -4)
                        (list -3) (list -2) (list -1) (list 0) (list 1) (list 2)
                        (list 3) (list 4) (list 5) (list 6) (list 7)))

(run-tests
 (@test-suite
  " liftBounds"
  (lambda () (display "Starting tests for liftBounds"))
  (lambda () (display "All tests for liftBounds passed!"))
 (@test-suite
  " liftBoundsExpr"
  (lambda () (display "Starting tests for liftBoundsExpr"))
  (lambda () (display "All tests for liftBoundsExpr passed!"))
  ; Checking atom case base case 
  (@test-case
   "TEST liftBoundsExpr on atom base case"
   (define sampleAtom (node/expr/atom empty-nodeinfo 1 'Node0))
   (@check-equal?
    (toString (liftBoundsExpr sampleAtom '() udt))
    (toString (list (list (node/expr/atom-name sampleAtom))))))

  ; Checking relation name case base case 
  (@test-case
   "TEST liftBoundsExpr on relation base case"
   (@check-equal?
    (toString (liftBoundsExpr Node '() udt))
    (toString nodeBound)))

  ; Checking Int constant case base case
  (@test-case
   "TEST liftBoundsExpr on int constant base case"
   (@check-equal?
    (toString (liftBoundsExpr varExprConstant '() udt))
    (toString intBounds)))

  ; Checking other esxpression constants base case
  ; UNIV
  (@test-case
   "TEST liftBoundsExpr on constant univ base case"
   (define expressionConstantUNIV (node/expr/constant empty-nodeinfo 1 'univ))
   (@check-equal?
    (toString (liftBoundsExpr expressionConstantUNIV '() udt))
    (toString (map (lambda (x) (list x x)) (forge:Run-atoms udt)))))

  ; IDEN
  (@test-case
   "TEST liftBoundsExpr on constant iden base case"
   (define expressionConstantIDEN (node/expr/constant empty-nodeinfo 1 'iden))
   (@check-equal?
    (toString (liftBoundsExpr expressionConstantIDEN '() udt))
    (toString (map (lambda (x) (list x x)) (forge:Run-atoms udt)))))

  ; Checking Quantified variable
  (@test-case
   "TEST lift bounds expr error"
   (define fSomeReachesAll
    (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsExpr fSomeReachesAll '() udt))))

  ; Checking Set Comprehension constant case
  (@test-case
   "TEST liftBoundsExpr on set comprehension case"
   (define qvx (node/expr/quantifier-var empty-nodeinfo 1 'x))
   (define fSetComprehension (node/expr/comprehension empty-nodeinfo 1
                                                      (list (cons qvx Node))
                                                      (in qvx Node)))
   (define uppersSetComprehension (list (liftBoundsExpr Node '(qvx) udt)))

   (@check-equal?
    (toString (liftBoundsExpr fSetComprehension '() udt))
    (toString (map (lambda (ub) (apply append ub))
                   (apply cartesian-product uppersSetComprehension))))))


 (@test-suite
  " liftBoundsExprOp"
  (lambda () (display "Starting tests for liftBoundsExprOp"))
  (lambda () (display "All tests for liftBoundsExprOp passed!"))
  ; Checking Set union case
  (@test-case
   "TEST liftBoundsExpr on set union case"
   (define uppersUnion (list nodeBound (map (lambda (x) (list x x))
                                            (forge:Run-atoms udt))))
   (@check-equal?
    (toString (liftBoundsExpr (+ Node univ) '() udt))
    (toString (remove-duplicates (apply append uppersUnion)))))

  ; Checking Set minus case
  (@test-case
   "TEST liftBoundsExpr on set minus case"
   (@check-equal?
    (toString (liftBoundsExpr (- Node univ) '() udt))
    (toString nodeBound)))

  ; Checking Set intersection case
  (@test-case
   "TEST liftBoundsExpr on set intersection case"
   (@check-equal?
    (toString (liftBoundsExpr (& Node (- Node (+ Node univ))) '() udt))
    (toString nodeBound)))

  ; Checking Set Product case
  (@test-case
   "TEST liftBoundsExpr on set product case"
   (define LHSProductBounds (liftBoundsExpr Node '() udt))
   (define RHSProductBounds (liftBoundsExpr (-> Node univ) '() udt))
   (debug-repl)
   (define listProductBounds (list LHSProductBounds RHSProductBounds))
   (define productMap (map (lambda (ub) (apply append ub))
                           (apply cartesian-product listProductBounds)))

   (@check-equal?
    (toString (liftBoundsExpr (-> Node (-> Node univ)) '() udt))
    (toString productMap)))

  ; Checking Set Join case
  ; Error case arity < 1
  (@check-exn
   exn:fail?
   (lambda () 
     (liftBoundsExpr (join Node Node) '() udt)))

  ; testing normal join case with two arguments  
  (@test-case
   "TEST liftBoundsExpr on join case"
   (define joinLHS (liftBoundsExpr edges '() udt))
   (define joinRHS (liftBoundsExpr iden '() udt))
   (define listJoin (list joinLHS joinRHS))
   (define newTuples (joinTuple (first listJoin) (second listJoin)))

   (@check-equal?
    (toString (liftBoundsExpr (join edges iden) '() udt))
    (toString newTuples)))


  ; Checking join with more than two arguments 
  (@test-case
   "TEST liftBoundsExpr on more complicated join case"
   (define joinLHSMore (liftBoundsExpr edges '() udt))
   (define joinRHSMore (liftBoundsExpr iden '() udt))
   (define listJoinMore (list joinLHSMore joinRHSMore joinRHSMore))
   (define newTuplesMore (joinTuple (first listJoinMore) (second listJoinMore)))
   (define foldNewTuples (foldl (lambda (curr acc)
                                  (joinTuple acc curr))
                                newTuplesMore (rest (rest listJoinMore))))

   (@check-equal?
    (toString (liftBoundsExpr (join edges iden iden) '() udt))
    (toString foldNewTuples)))

  ; Checking Set transitive closure case
  (@test-case
   "TEST liftBoundsExpr on set transitive closure case"
   (define transitiveClosureBounds (liftBoundsExpr edges '() udt))
   (@check-equal?
    (toString (liftBoundsExpr (^ edges) '() udt))
    (toString (buildClosureOfTupleSet transitiveClosureBounds))))

  ; Checking Set reflexive transitive closure case
  (@test-case
   "TEST liftBoundsExpr on set reflexive transitive closure case"
   (define reflexiveTransitiveClosureBounds (liftBoundsExpr edges '() udt))
   (define closureOfTupleSets
     (buildClosureOfTupleSet reflexiveTransitiveClosureBounds))

   (@check-equal?
    (toString (liftBoundsExpr (* edges) '() udt))
    (toString (remove-duplicates
               (append closureOfTupleSets
                       (map (lambda (x) (list x x)) (forge:Run-atoms udt)))))))


  ; Checking Set transpose case
  (@test-case
   "TEST liftBoundsExpr on set transpose case"
   (define transposeBounds (list (liftBoundsExpr edges '() udt)))
   (@check-equal?
    (toString (liftBoundsExpr (~ edges) '() udt))
    (toString (map (lambda (x) (transposeTup x)) (first transposeBounds)))))

  ; Checking Set singleton case
  (@test-case
   "TEST liftBoundsExpr on set singleton case"
   (@check-equal?
    (toString (liftBoundsExpr (sing varIntConstX) '() udt))
    (toString intBounds))))

 (@test-suite
  " liftBoundsInt"
  (lambda () (display "Starting tests for liftBoundsInt"))
  (lambda () (display "All tests for liftBoundsInt passed!"))
  ; Checking const int case
  (@test-case
   "TEST liftBoundsExpr on const int case"
   (@check-equal?
    (toString (liftBoundsInt varIntConstX '() udt))
    (toString intBounds)))

  ; Checking int with operator (should error)
  (@test-case
   "TEST liftBoundsInt error int with <"
   (define fIntLess (< varIntConstX varIntConstY))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt fIntLess '() udt))))

  (@test-case
   "TEST liftBoundsInt error with >"
   (define fIntGreater (> varIntConstX varIntConstY))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt fIntGreater '() udt))))

  ; Checking sum "quantifier" case
  (@test-case
   "TEST liftBoundsExpr on sum quantifier case"
   (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
   (define fSum (node/int/sum-quant empty-nodeinfo
                                    (list (cons x Node))
                                    (node/int/op/card empty-nodeinfo
                                                      (list (join edges x)))))
   (@check-equal?
    (toString (liftBoundsInt fSum '() udt))
    (toString intBounds)))

  ;Checking cardinality case
  (@test-case
   "TEST liftBoundsExpr on cardinality case"
   (define fCardinality (node/int/op/card empty-nodeinfo (list Node)))
   (@check-equal?
    (toString (liftBoundsInt fCardinality '() udt))
    (toString intBounds))))

 (@test-suite
  " liftBoundsIntOp"
  (lambda () (display "Starting tests for liftBoundsIntOp"))
  (lambda () (display "All tests for liftBoundsIntOp passed!"))
  ; sum

  (@test-case
   "TEST liftBoundsInt error with sum"
   (define intSum (sum edges))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intSum '() udt))))

  ; lift-bounds-op
  ; int addition
  (@test-case
   "TEST liftBoundsInt error with +"
   (define intAdd (+ 1 2))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intAdd '() udt #t))))

  ; int subtraction
  (@test-case
   "TEST liftBoundsInt error with -"
   (define intSub (- 1 2))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intSub '() udt #t))))

  ; int division
  (@test-case
   "TEST liftBoundsInt error with /"
   (define intDiv (/ 1 2))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intDiv '() udt #t))))

  ; int mult
  (@test-case
   "TEST liftBoundsInt error with *"
   (define intMult (* 1 2))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intMult '() udt #t))))

  ; int sum
  (@test-case
   "TEST liftBoundsInt error with sum"
   (define intSumErr  (sum Node))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intSumErr  '() udt #t))))

  ; int mod
  (@test-case
   "TEST liftBoundsInt error with mod"
   (define intMod (modulo 0 5))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intMod '() udt #t))))

  ; int abs
  ; TODO: finish this case
  #|(define int-abs (absolute 1))
(@check-exn
 exn:fail?
 (lambda () 
   (liftBoundsInt int-abs '() udt #t)))|#

  ; int sign-of
  (@test-case
   "TEST liftBoundsInt error with sgn"
   (define intSignOf (sgn 1))
   (@check-exn
    exn:fail?
    (lambda () 
      (liftBoundsInt intSignOf '() udt #t)))))))
