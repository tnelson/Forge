#lang forge/core

(require "../desugar/desugar.rkt")
(require "forge_ex.rkt")
(require "test_helpers.rkt")
(require "../desugar/desugar_helpers.rkt")
(require "../lift-bounds/lift-bounds.rkt")
(require (prefix-in @ rackunit))
(require rackunit/text-ui)

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])


(run-tests (@test-suite 
            " Desugar tests"
            (lambda () (display "Starting tests for Desugar"))
            (lambda () (display "All tests for Desugar passed!"))

            ; constant formulas
            (@test-case
             "TEST 1 constant formulas"
             (define const (node/formula/constant empty-nodeinfo Int))
             (@check-equal?
              (desugarFormula const '() udt #t)
              const))

            ; multiplicity formula
            ; I think this one is working (-ABBY)
            ;(define fSomeReachesAll (one edges))
            ;(desugarFormula fSomeReachesAll '() udt #t)

            ; quantified formula

            ; no
            ; QUESTION: THESE RETURN VERY LONG THINGS
            #|(@test-case
              "TEST NO formula currSign true"
             (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
             (define y (node/expr/quantifier-var empty-nodeinfo 1 'y))
             (define noTest (no ([x Node]) (all ([y Node]) (in y (join x edges)))))
             (define negatedFormula  (node/formula/op/! empty-nodeinfo (list (in y (join x edges)))))
             (define newQuantFormula (node/formula/quantified empty-nodeinfo 'all (list [y Node] [x Node]) negatedFormula))
             (@check-equal?
              (toString (desugarFormula noTest '() udt #t))
              (toString (desugarFormula newQuantFormula '() udt #t))))|#

            ; one
            ;(define oneTest (one ([x Node]) (in x Node)))

            ; lone
            ; (define loneTest (lone ([x Node])  (in x Node)))

            ; AND
            (@test-case
             "TEST AND formula currSign true"
             (define andTest (and true false))
             (@check-equal?
              (desugarFormula andTest '() udt #t)
              (node/formula/op/&& empty-nodeinfo (list true false))))


            (@test-case
             "TEST AND formula currSign false"
             (define andTest (and true false))
             (@check-equal?
              (desugarFormula andTest '() udt #f)
              (node/formula/op/|| empty-nodeinfo (list false true))))

            ; OR
            (@test-case
             "TEST OR formula currSign true"
             (define orTest (or true false))
             (@check-equal?
              (desugarFormula orTest '() udt #t)
              (node/formula/op/|| empty-nodeinfo (list true false))))

            (@test-case
             "TEST OR formula currSign false"
             (define orTest (or true false))
             (@check-equal?
              (desugarFormula orTest '() udt #f)
              (node/formula/op/&& empty-nodeinfo (list false true))))

            ; IMPLIES
            (@test-case
             "TEST implies formula"
             (define impliesTest (implies true false))
             (@check-equal?
              (desugarFormula impliesTest '() udt #t)
              ;desugars to (not LHS) OR (RHS) 
              (node/formula/op/|| empty-nodeinfo (list false false))))

            ; IN For Not Ground LeftE
            (@test-case
             "TEST In formula Node relation in univ"
             (define inTest (in Node univ))
             (define leftE Node)
             (define rightE univ)
             (define liftedUpperBounds (liftBoundsExpr leftE '() udt))
             (define desugaredAnd
               (node/formula/op/&& empty-nodeinfo
                                   (map (lambda (x)
                                          (define tupExpr
                                            (tup2Expr x udt empty-nodeinfo))
                                          (define LHS
                                            (node/formula/op/in
                                             empty-nodeinfo
                                             (list tupExpr leftE)))
                                          (define RHS
                                            (node/formula/op/in
                                             empty-nodeinfo
                                             (list tupExpr rightE)))
                                          (node/formula/op/=> empty-nodeinfo
                                                              (list LHS RHS)))
                                        liftedUpperBounds))) 
        
             (@check-equal?
              (desugarFormula inTest '() udt #t)
              (desugarFormula desugaredAnd '() udt #t)))

            ; EQUALS
            (@test-case
             "TEST EQUALS formula"
             (define equalsTest (= Node Node))
             (define LHS (node/formula/op/in empty-nodeinfo (list Node Node)))
             (define RHS (node/formula/op/in empty-nodeinfo (list Node Node)))
             (define desugaredEquals (node/formula/op/&& empty-nodeinfo (list LHS RHS)))
             (@check-equal?
              (desugarFormula equalsTest '() udt #t)
              (desugarFormula  desugaredEquals '() udt #t)))

            ; NEGATION
            (@test-case
             "TEST NEGATION formula"
             (define negationTest (! true))
             (@check-equal?
              (desugarFormula negationTest '() udt #t)
              false))

            ; INTEGER <
            (@test-case
             "TEST error <"
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intLess (< varIntConstX varIntConstY ))
                (desugarFormula intLess '() udt #t))))

            ; INTEGER >
            (@test-case
             "TEST error >"
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intGreater (> varIntConstX varIntConstY ))
                (desugarFormula intGreater '() udt #t))))

            ; INTEGER =
            (@test-case
             "TEST error ="
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intEquals (= varIntConstX varIntConstY ))
                (desugarFormula intEquals '() udt #t))))


            ; error test desugarExpr no expr
            (@test-case
             "TEST error desugarExpr"
             (@check-exn
              exn:fail?
              (lambda ()
                (desugarExpr '() '() '() udt #f))))
            
            ; relation name
            (@test-case
             "TEST RELATION NAME"
             (define relationTest Node)
             (@check-equal?
              (desugarExpr relationTest '() '(Node) udt #f)
              (node/formula/op/in
               empty-nodeinfo
               (list
                (node/expr/op/->
                 empty-nodeinfo
                 1 (list (node/expr/atom empty-nodeinfo 1 'Node))) Node))))

            ; atom currSign true
            (@test-case
             "TEST atom on currSign true"
             (define result (node/formula/op/in
                             empty-nodeinfo
                             (list (tup2Expr '(Node) udt empty-nodeinfo)
                                   (node/expr/atom empty-nodeinfo 1 'Node))))
             (@check-equal?
              (desugarExpr
               (node/expr/atom empty-nodeinfo 1 'Node)
               '() '(Node) udt #t)
              result))

            ; atom currSign false
            (@test-case
             "TEST atom on currSign false"
             (define result (node/formula/op/!
                             empty-nodeinfo
                             (node/formula/op/in
                              empty-nodeinfo
                              (list
                               (tup2Expr
                                '(Node) udt empty-nodeinfo)
                               (node/expr/atom empty-nodeinfo 1 'Node)))))
             (@check-equal?
              (desugarExpr
               (node/expr/atom empty-nodeinfo 1 'Node)
               '() '(Node) udt #f)
              result))

            ; Int constant
            (@test-case
             "TEST Int"
             (define result (node/formula/op/in
                             empty-nodeinfo
                             (list
                              (tup2Expr '(Node) udt empty-nodeinfo)
                              (node/expr/constant empty-nodeinfo 1 'Int))))
             (@check-equal?
              (desugarExpr
               (node/expr/constant empty-nodeinfo 1 'Int)
               '() '(Node) udt #f)
              result))

            ; other expression constants

            ; quantified variable


            ; set comprehension desugar formula
            (@test-case
             "TEST desugar in set comprehension case called from desugar formula"

             (define fSetComprehension (set ([x Node]) (in x Node)))
             
             (define inSetComprehension (in (atom 'Node0) fSetComprehension))

             (define liftedUpperBounds (liftBoundsExpr  (atom 'Node0) '() udt))

             (define tupExpr (tup2Expr (first liftedUpperBounds) udt empty-nodeinfo))
           
             (@check-equal?
              (desugarFormula inSetComprehension '() udt #t)
              (and (in tupExpr Node) (in tupExpr Node))))

            ; desugarExpr test on no currTupIfAtomic
            (@check-exn
             exn:fail?
             (lambda () 
               (desugarFormula (+ Node Node) '() '() udt #t)))

            ; UNION
            (@test-case
             "TEST UNION expression"
             (define unionTest (+ Node Node))
             (define desugaredChild (desugarExpr Node '() '(Node) udt #t))
             (define desugaredWithOr
               (node/formula/op/|| empty-nodeinfo
                                   (list desugaredChild desugaredChild)))
             (@check-equal?
              (desugarExpr unionTest '() '(Node) udt #t)
              desugaredWithOr))


            ; SETMINUS
            ; (currTupIfAtomic in LHS) and (not(currTupIfAtomic in RHS))
            #|(define setMinusTest (- Node Node))
            (define LHSSetMinus (node/formula/op/in info (list currTupIfAtomicExpr (first args))))
            (define RHSSetMinus (node/formula/op/! info (list node/formula/op/in info (list currTupIfAtomicExpr (second args)))))
            (define desugaredSetMinus (node/formula/op/&& info (list LHSSetMinus RHSSetMinus)))
            (@test-case
             "TEST SETMINUS expression"
             (@check-equal?
              (toString (desugarExpr setMinusTest '() '(Node) udt #t))
              (toString (desugarFormula desugaredSetMinus quantVars runContext currSign))))|#

            ; INTERSECTION

            ; PRODUCT
            (@test-case
             "TEST PRODUCT expression on arity 2"
             (define productTest (-> Node Node))
             (define sol
               (node/formula/op/&&
                empty-nodeinfo
                (list (node/formula/op/in
                       empty-nodeinfo
                       (list (node/expr/op/->
                              empty-nodeinfo 1
                              (list
                               (node/expr/atom empty-nodeinfo 1 'Node))) Node))
                      (node/formula/op/in
                       empty-nodeinfo
                       (list (node/expr/op/->
                              empty-nodeinfo 1
                              (list
                               (node/expr/atom empty-nodeinfo 1 'Node)))
                             Node)))))
             (@check-equal?
              (desugarExpr productTest '() '(Node) udt #t)
              sol))

            (@test-case
             "TEST PRODUCT expression on arity 3"
             (define productTest (-> Node Node Node))
             (define sol
               (node/formula/op/&&
                empty-nodeinfo
                (list (node/formula/op/in
                       empty-nodeinfo
                       (list (node/expr/op/->
                              empty-nodeinfo 1
                              (list
                               (node/expr/atom empty-nodeinfo 1 'Node))) Node))
                      (node/formula/op/in
                       empty-nodeinfo
                       (list (node/expr/op/->
                              empty-nodeinfo 1
                              (list
                               (node/expr/atom empty-nodeinfo 1 'Node)))
                             (node/expr/op/->
                              empty-nodeinfo 3
                              (list Node Node))
                             )))))
             (@check-equal?
              (desugarExpr productTest '() '(Node) udt #t)
              sol))

            ; JOIN
            #|(printf "TEST!!!!!!!")
 (define joinFormulaORFalse (in (node/expr/atom empty-nodeinfo 1 'Node0)
                         (join Node edges)))
(desugarFormula joinFormulaORFalse '() udt #f)

(define joinFormulaORTrue (in (node/expr/atom empty-nodeinfo 1 'Node0)
                         (join Node edges)))
(desugarFormula joinFormulaORTrue '() udt #t)

(define joinFormulaORFalseBiggerArity (in (node/expr/atom empty-nodeinfo 1 'Node0)
                         (join Node edges edges)))
(desugarFormula joinFormulaORFalseBiggerArity '() udt #t) |#

            ; TRANSITIVE CLOSURE
            #|(define fSomeReachesAll
  (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(desugarFormula fSomeReachesAll '() udt #t)|#
            (@test-case
             "TEST JOIN on OR with currSign False 2 arguments"
             (define joinFormulaORFalse (in (node/expr/atom empty-nodeinfo 1 'Node0)
                                            (join Node edges)))
             (@check-equal?
              (desugarFormula joinFormulaORFalse '() udt #f)
              (node/formula/op/&&
               empty-nodeinfo
               (list (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node0)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))
                     (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node1)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node1)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))
                     (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node2)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node2)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))
                     (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node3)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node3)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))
                     (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node4)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node4)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))
                     (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node5)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node5)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))
                     (node/formula/op/||
                      empty-nodeinfo
                      (list (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    1 (list (node/expr/atom empty-nodeinfo 1 'Node6)))
                                   (rel '(Node) 'univ "Node")))
                            (node/formula/op/in
                             empty-nodeinfo
                             (list (node/expr/op/->
                                    empty-nodeinfo
                                    2 (list (node/expr/atom empty-nodeinfo 1 'Node6)
                                            (node/expr/atom empty-nodeinfo 1 'Node0)))
                                   (rel '(Node Node) 'Node "edges")))))))))
            ; REFLEXIVE-TRANSITIVE CLOSURE

            ; TRANSPOSE

            ; SINGLETON

            ;;;;;;;;;;;;;;;;;; DESUGAR INT ;;;;;;;;;;;;;;;;;;;;

            ; CONSTANT INT
            (@test-case
             "TEST intConstant failure"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intConstFail (node/int/constant empty-nodeinfo 1))
                (desugarInt intConstFail '() udt))))

            ; sum
            (@test-case
             "TEST sum error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSum (sum edges))
                (desugarInt intSum '() udt))))

            ; desugarIntOp
            ; int addition
            (@test-case
             "TEST addition error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intAdd (+ 1 2))
                (desugarInt intAdd '() udt #t))))

            ; int subtraction
            (@test-case
             "TEST sub error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSub (- 1 2))
                (desugarInt intSub '() udt #t))))

            ; int division
            (@test-case
             "TEST div error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intDiv (/ 1 2))
                (desugarInt intDiv '() udt #t))))

            ; int mult
            (@test-case
             "TEST mult error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intMult (* 1 2))
                (desugarInt intMult '() udt #t))))

            ; int sum
            (@test-case
             "TEST sum error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSumErr (sum Node))
                (desugarInt intSumErr '() udt #t))))

            ; int card
            (@test-case
             "TEST card error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intCard (card Node))
                (desugarInt intCard '() udt #t))))

            ; int mod
            (@test-case
             "TEST mod error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intMod (modulo 0 5))
                (desugarInt intMod '() udt #t))))

            ; int abs
            ; TODO: finish this case 
            #|(@check-exn
             exn:fail?
             (lambda ()
               (define intAbs (absolute 1))
               (desugarInt intAbs '() udt #t)))|#

            ; int sign-of
            (@test-case
             "TEST signOf error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSignOf (sgn 1))
                (desugarInt intSignOf '() udt #t))))))