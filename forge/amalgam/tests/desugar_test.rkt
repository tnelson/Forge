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
              (first (desugarFormula const '() udt))
              const))

            ; no
            (@test-case
              "TEST NO formula currSign true"
             (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
             (define noTest (node/formula/quantified empty-nodeinfo 'no '(x Node) (in x x)))
             (@check-equal?
              (first (desugarFormula noTest '() udt))
              (node/formula/quantified empty-nodeinfo 'all '(x Node) (! (in x x)))))

            ; lone
            (@test-case
             "TEST LONE formula currSign true"
             (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
             (define loneTest (node/formula/quantified empty-nodeinfo 'lone (list (cons x Node)) (in x x)))
             (@check-equal?
              (first (desugarFormula loneTest '() udt))
              (or (node/formula/quantified empty-nodeinfo 'no (list (cons x Node)) (in x x))
                  (node/formula/quantified empty-nodeinfo 'one (list (cons x Node)) (in x x)))))

            ; implies
            (@test-case
             "TEST implies formula"
             (define impliesTest (implies true false))
             (@check-equal?
              (first (desugarFormula impliesTest '() udt))
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
              (first (desugarFormula inTest '() udt))
              desugaredAnd))

            (@test-case
             "TEST EQUALS formula"
             (define equalsTest (= Node Node))
             (define LHS (in (list Node Node)))
             (define RHS (in (list Node Node)))
             (define desugaredEquals (&& (list LHS RHS)))
             (@check-equal?
              (first (desugarFormula equalsTest '() udt))
              desugaredEquals))

            (@test-case
             "TEST error <"
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intLess (< varIntConstX varIntConstY ))
                (desugarFormula intLess '() udt))))

            (@test-case
             "TEST error >"
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intGreater (> varIntConstX varIntConstY ))
                (desugarFormula intGreater '() udt))))

            (@test-case
             "TEST error ="
             (@check-exn
              exn:fail?
              (lambda ()
                (define varIntConstX (node/int/constant empty-nodeinfo 1))
                (define varIntConstY  (node/int/constant empty-nodeinfo 2))
                (define intEquals (= varIntConstX varIntConstY ))
                (desugarFormula intEquals '() udt))))

            (@test-case
             "TEST error desugarExpr"
             (@check-exn
              exn:fail?
              (lambda ()
                (desugarExpr '() '() '() udt))))
            
            (@test-case
             "TEST RELATION NAME"
             (define relationTest Node)
             (@check-equal?
              (first (desugarExpr relationTest '() '(Node0) udt))
              (in (list (node/expr/atom empty-nodeinfo 1 'Node0) Node))))

            (@test-case
             "TEST atom base case"
             (define result (in (list (tup2Expr '(Node) udt empty-nodeinfo)
                                      (node/expr/atom empty-nodeinfo 1 'Node))))
             (@check-equal?
              (first (desugarExpr
               (node/expr/atom empty-nodeinfo 1 'Node)
               '() '(Node) udt))
              result))
            
            (@test-case
             "TEST Int"
             (@check-equal?
              (first (desugarExpr
               (node/expr/constant empty-nodeinfo 1 'Int)
               '() '(Node0) udt))
              (in (tup2Expr '(Node0) udt empty-nodeinfo) (node/expr/constant empty-nodeinfo 1 'Int))))

            (@test-case
             "TEST desugar in set comprehension case called from desugar formula"

             (define fSetComprehension (set ([x Node]) (in x Node)))
             
             (define inSetComprehension (in (atom 'Node0) fSetComprehension))

             (define liftedUpperBounds (liftBoundsExpr  (atom 'Node0) '() udt))

             (define tupExpr (tup2Expr (first liftedUpperBounds) udt empty-nodeinfo))
           
             (@check-equal?
              (first (desugarFormula inSetComprehension '() udt))
              (and (in tupExpr Node) (in tupExpr Node))))

            (@check-exn
             exn:fail?
             (lambda () 
               (desugarFormula (+ Node Node) '() '() udt)))

            (@test-case
             "TEST UNION expression"
             (define unionTest (+ Node Node))
             (define desugaredChild (first (desugarExpr Node '() '(Node) udt)))
             (define desugaredWithOr
               (|| (list desugaredChild desugaredChild)))
             (@check-equal?
              (first (desugarExpr unionTest '() '(Node) udt))
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
              (first (desugarExpr setMinusTest '() '(Node0) udt))
              desugaredSetMinus))

            (@test-case
             "Test INTERSECTION expression"
             (define intersectionTest (& Node Node))
             (@check-equal?
              (first (desugarExpr intersectionTest '() '(Node0) udt))
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
              (first (desugarExpr productTest '() '(Node0 Node0) udt))
              sol))

            (@test-case
             "TEST PRODUCT expression on arity 3"
             (define productTest (-> Node Node Node))
             (@check-equal?
              (first (desugarExpr productTest '() '(Node0 Node1 Node2) udt))
              (first (desugarExpr (-> Node (-> Node Node)) '() '(Node0 Node1 Node2) udt))))

            ;JOIN 
            (@test-case
             "TEST JOIN on OR with currSign False 2 arguments"
             (define joinFormula (join Node edges))
             (@check-equal?
              (first (desugarExpr joinFormula '() '(Node0) udt))
              (|| (list (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node0)
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
                        (&& (list (in (list (node/expr/atom empty-nodeinfo 1 'Node3)
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
             "TEST transitive closure expression"
             
             (define (createOutput)
               (define a (list 'Node0 'Node1 'Node2 'Node3 'Node4 'Node5 'Node6))
               (define product (cartesian-product a a))
               (&& (map (lambda (pair) (=>
                                        (in (-> (atom (first pair))
                                                (atom (second pair))) (^ edges))
                                        (in (-> (atom (first pair))
                                                (atom (second pair)))
                                            (^ edges)))) product)))

             (define transitiveClosure (^ edges))
             (@check-equal?
              (first (desugarFormula (in transitiveClosure transitiveClosure) '() udt))
              (createOutput)))

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
              (first (desugarExpr reflexiveEx '() '(Node0 Node1) udt))
              inFormula))
            
            (@test-case
             "TEST Transpose expression"
             (@check-equal?
              (first (desugarExpr (~ edges) '() '(Node0 Node1) udt))
              (first (desugarExpr edges '() '(Node1 Node0) udt))))

            (@test-case
             "TEST singleton error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define singletonEx (node/expr/op/sing empty-nodeinfo 1 (list 1)))
                (desugarExpr singletonEx '() '(Node) udt))))

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
                (desugarInt intAdd '() udt))))

            (@test-case
             "TEST sub error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSub (- 1 2))
                (desugarInt intSub '() udt))))

            (@test-case
             "TEST div error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intDiv (/ 1 2))
                (desugarInt intDiv '() udt))))

            (@test-case
             "TEST mult error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intMult (* 1 2))
                (desugarInt intMult '() udt))))

            (@test-case
             "TEST sum error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSumErr (sum Node))
                (desugarInt intSumErr '() udt))))

            (@test-case
             "TEST card error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intCard (card Node))
                (desugarInt intCard '() udt))))

            (@test-case
             "TEST mod error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intMod (modulo 0 5))
                (desugarInt intMod '() udt))))

            (@check-exn
             exn:fail?
             (lambda ()
               (define intAbs (abs 1))
               (desugarInt intAbs '() udt)))

            (@test-case
             "TEST signOf error"
             (@check-exn
              exn:fail?
              (lambda ()
                (define intSignOf (sgn 1))
                (desugarInt intSignOf '() udt))))))