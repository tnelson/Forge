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

            (@test-case
             "TEST implies formula"
             (define impliesTest (implies true false))
             (@check-equal?
              (desugarFormula impliesTest '() udt #t)
              (|| (list (not true) false))))

            (@test-case
             "TEST In formula Node relation in univ"
             (define inTest (in Node univ))
             (define leftE Node)
             (define rightE univ)
             (define liftedUpperBounds (liftBoundsExpr leftE '() udt))
             (define desugaredAnd
               (&& (map (lambda (x)
                          (define tupExpr
                            (tup2Expr x udt empty-nodeinfo))
                          (define LHS
                            (in (list tupExpr leftE)))
                          (define RHS
                            (in (list tupExpr rightE)))
                          (=>/info empty-nodeinfo (list LHS RHS)))
                        liftedUpperBounds))) 
        
             (@check-equal?
              (desugarFormula inTest '() udt #t)
              desugaredAnd))

            (@test-case
             "TEST EQUALS formula"
             (define equalsTest (= Node Node))
             (define LHS (in (list Node Node)))
             (define RHS (in (list Node Node)))
             (define desugaredEquals (&& (list LHS RHS)))
             (@check-equal?
              (desugarFormula equalsTest '() udt #t)
              desugaredEquals))

            (@test-case
             "TEST error <"
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intLess (< varIntConstX varIntConstY ))
                (desugarFormula intLess '() udt #t))))

            (@test-case
             "TEST error >"
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intGreater (> varIntConstX varIntConstY ))
                (desugarFormula intGreater '() udt #t))))

            (@test-case
             "TEST error ="
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intEquals (= varIntConstX varIntConstY ))
                (desugarFormula intEquals '() udt #t))))

            (@test-case
             "TEST error desugarExpr"
             (@check-exn
              exn:fail?
              (lambda ()
                (desugarExpr '() '() '() udt #f))))
            
            (@test-case
             "TEST RELATION NAME"
             (define relationTest Node)
             (@check-equal?
              (desugarExpr relationTest '() '(Node0) udt #t)
              (in (list (node/expr/atom empty-nodeinfo 1 'Node0) Node))))

            (@test-case
             "TEST atom on currSign true"
             (define result (in (list (tup2Expr '(Node) udt empty-nodeinfo)
                                      (node/expr/atom empty-nodeinfo 1 'Node))))
             (@check-equal?
              (desugarExpr
               (node/expr/atom empty-nodeinfo 1 'Node)
               '() '(Node) udt #t)
              result))

            (@test-case
             "TEST atom on currSign false"
             (define result (!/info
                             empty-nodeinfo
                             (list (node/formula/op/in empty-nodeinfo
                                                       (list (tup2Expr
                                                              '(Node) udt empty-nodeinfo)
                                                             (node/expr/atom empty-nodeinfo 1 'Node))))))
             (@check-equal?
              (desugarExpr
               (node/expr/atom empty-nodeinfo 1 'Node)
               '() '(Node) udt #f)
              result))
            
            (@test-case
             "TEST Int"
             (@check-equal?
              (desugarExpr
               (node/expr/constant empty-nodeinfo 1 'Int)
               '() '(Node0) udt #f)
              (not (in (tup2Expr '(Node0) udt empty-nodeinfo) (node/expr/constant empty-nodeinfo 1 'Int)))))

            (@test-case
             "TEST desugar in set comprehension case called from desugar formula"

             (define fSetComprehension (set ([x Node]) (in x Node)))
             
             (define inSetComprehension (in (atom 'Node0) fSetComprehension))

             (define liftedUpperBounds (liftBoundsExpr  (atom 'Node0) '() udt))

             (define tupExpr (tup2Expr (first liftedUpperBounds) udt empty-nodeinfo))
           
             (@check-equal?
              (desugarFormula inSetComprehension '() udt #t)
              (and (in tupExpr Node) (in tupExpr Node))))

            (@check-exn
             exn:fail?
             (lambda () 
               (desugarFormula (+ Node Node) '() '() udt #t)))

            (@test-case
             "TEST UNION expression"
             (define unionTest (+ Node Node))
             (define desugaredChild (desugarExpr Node '() '(Node) udt #t))
             (define desugaredWithOr
               (|| (list desugaredChild desugaredChild)))
             (@check-equal?
              (desugarExpr unionTest '() '(Node) udt #t)
              desugaredWithOr))

            (@test-case
             "TEST SETMINUS expression"
             (define setMinusTest (- Node Node))
             (define currTupIfAtomicExpr (tup2Expr '(Node0) udt empty-nodeinfo))
             (define args (list Node Node))
             (define LHSSetMinus (in (list currTupIfAtomicExpr (first args))))
             (define RHSSetMinus
               (!/info empty-nodeinfo
                       (list (in (list currTupIfAtomicExpr (second args))))))
             (define desugaredSetMinus (&& (list LHSSetMinus RHSSetMinus)))
             (@check-equal?
              (desugarExpr setMinusTest '() '(Node0) udt #t)
              desugaredSetMinus))

            (@test-case
             "Test INTERSECTION expression"
             (define intersectionTest (& Node Node))
             (@check-equal?
              (desugarExpr intersectionTest '() '(Node0) udt #t)
              (&&
               (list
                (in
                 (list (node/expr/atom empty-nodeinfo 1 'Node0)
                       (rel '(Node) 'univ "Node")))
                (in 
                 (list
                  (node/expr/atom empty-nodeinfo 1 'Node0)
                  (rel '(Node) 'univ "Node")))))))
            
            (@test-case
             "TEST PRODUCT expression on arity 2"
             (define productTest (-> Node Node))
             (define
               leftTupleContext
               (projectTupleRange '(Node0 Node0) 0 (node/expr-arity Node)))
             (define
               rightTupleContext
               (projectTupleRange '(Node0 Node0)
                                  (node/expr-arity Node) 2))
             (define sol
               (and
                (in (tup2Expr leftTupleContext udt empty-nodeinfo) Node)
                (in (tup2Expr rightTupleContext udt empty-nodeinfo) Node)))
             (@check-equal?
              (desugarExpr productTest '() '(Node0 Node0) udt #t)
              sol))

            (@test-case
             "TEST PRODUCT expression on arity 3"
             (define productTest (-> Node Node Node))
             (@check-equal?
              (desugarExpr productTest '() '(Node0 Node1 Node2) udt #t)
              (desugarExpr (-> Node (-> Node Node)) '() '(Node0 Node1 Node2) udt #t)))

            ;JOIN 
            (@test-case
             "TEST JOIN on OR with currSign False 2 arguments"
             (define joinFormula (join Node edges))
             (@check-equal?
              (desugarExpr joinFormula '() '(Node0) udt #f)
              (|| (list (&&(list (in (list (node/expr/atom empty-nodeinfo 1 'Node0)
                                           (rel '(Node) 'univ "Node")))
                                 (in (list (node/expr/op/->
                                            empty-nodeinfo
                                            2 (list (node/expr/atom empty-nodeinfo 1 'Node0)
                                                    (node/expr/atom empty-nodeinfo 1 'Node0)))
                                           (rel '(Node Node) 'Node "edges")))))
                        (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node1)
                                            (rel '(Node) 'univ "Node")))
                                  (in (list (node/expr/op/->
                                             empty-nodeinfo
                                             2 (list (node/expr/atom empty-nodeinfo 1 'Node1)
                                                     (node/expr/atom empty-nodeinfo 1 'Node0)))
                                            (rel '(Node Node) 'Node "edges")))))
                        (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node2)
                                            (rel '(Node) 'univ "Node")))
                                  (in (list (node/expr/op/->
                                             empty-nodeinfo
                                             2 (list (node/expr/atom empty-nodeinfo 1 'Node2)
                                                     (node/expr/atom empty-nodeinfo 1 'Node0)))
                                            (rel '(Node Node) 'Node "edges")))))
                        (&&(list (in (list (node/expr/atom empty-nodeinfo 1 'Node3)
                                           (rel '(Node) 'univ "Node")))
                                 (in (list (node/expr/op/->
                                            empty-nodeinfo
                                            2 (list (node/expr/atom empty-nodeinfo 1 'Node3)
                                                    (node/expr/atom empty-nodeinfo 1 'Node0)))
                                           (rel '(Node Node) 'Node "edges")))))
                        (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node4)
                                            (rel '(Node) 'univ "Node")))
                                  (in (list (node/expr/op/->
                                             empty-nodeinfo
                                             2 (list (node/expr/atom empty-nodeinfo 1 'Node4)
                                                     (node/expr/atom empty-nodeinfo 1 'Node0)))
                                            (rel '(Node Node) 'Node "edges")))))
                        (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node5)
                                            (rel '(Node) 'univ "Node")))
                                  (in (list (node/expr/op/->
                                             empty-nodeinfo
                                             2 (list (node/expr/atom empty-nodeinfo 1 'Node5)
                                                     (node/expr/atom empty-nodeinfo 1 'Node0)))
                                            (rel '(Node Node) 'Node "edges")))))
                        (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node6)
                                            (rel '(Node) 'univ "Node")))
                                  (in (list (node/expr/op/->
                                             empty-nodeinfo
                                             2 (list (node/expr/atom empty-nodeinfo 1 'Node6)
                                                     (node/expr/atom empty-nodeinfo 1 'Node0)))
                                            (rel '(Node Node) 'Node "edges")))))))))

            (@test-case
             "TEST reflexive-transitive closure expression"
             (define transitiveClosure
               (^/info empty-nodeinfo (list (first (node/expr/op-children (* edges))))))
             (define desugaredRClosure
               (+/info empty-nodeinfo (list iden transitiveClosure)))
             (define inFormula
               (node/formula/op/in empty-nodeinfo
                                   (list (tup2Expr '(Node0 Node1) udt empty-nodeinfo)
                                         desugaredRClosure)))
             (define reflexiveEx (* edges))
             (@check-equal?
              (desugarExpr reflexiveEx '() '(Node0 Node1) udt #t)
              inFormula))
            
            (@test-case
             "TEST Transpose expression"
             (@check-equal?
              (desugarExpr (~ edges) '() '(Node0 Node1) udt #t)
              (desugarExpr edges '() '(Node1 Node0) udt #t)))

            (@test-case
             "TEST singleton error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define singletonEx (node/expr/op/sing empty-nodeinfo 1 (list 1)))
                (desugarExpr singletonEx '() '(Node) udt #t))))

            (@test-case
             "TEST intConstant failure"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intConstFail (node/int/constant empty-nodeinfo 1))
                (desugarInt intConstFail '() udt))))

            (@test-case
             "TEST sum error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSum (sum edges))
                (desugarInt intSum '() udt))))

            (@test-case
             "TEST addition error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intAdd (+ 1 2))
                (desugarInt intAdd '() udt #t))))

            (@test-case
             "TEST sub error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSub (- 1 2))
                (desugarInt intSub '() udt #t))))

            (@test-case
             "TEST div error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intDiv (/ 1 2))
                (desugarInt intDiv '() udt #t))))

            (@test-case
             "TEST mult error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intMult (* 1 2))
                (desugarInt intMult '() udt #t))))

            (@test-case
             "TEST sum error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSumErr (sum Node))
                (desugarInt intSumErr '() udt #t))))

            (@test-case
             "TEST card error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intCard (card Node))
                (desugarInt intCard '() udt #t))))

            (@test-case
             "TEST mod error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intMod (modulo 0 5))
                (desugarInt intMod '() udt #t))))

            (@check-exn
             exn:fail?
             (lambda ()
               (define intAbs (abs 1))
               (desugarInt intAbs '() udt #t)))

            (@test-case
             "TEST signOf error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSignOf (sgn 1))
                (desugarInt intSignOf '() udt #t))))))