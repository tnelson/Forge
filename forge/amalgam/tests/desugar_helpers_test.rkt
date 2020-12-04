#lang forge/core
(require "../desugar/desugar_helpers.rkt")
(require "../desugar/desugar.rkt")
(require "../substitutor/substitutor.rkt")
(require "forge_ex.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))
(require rackunit/text-ui)

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(run-tests (@test-suite 
            " Desugar helpers tests"
            (lambda () (display "Starting tests for Desugar helpers"))
            (lambda () (display "All tests for Desugar helpers passed!"))

            ; productHelper
            (@test-case
             "TEST productHelper on valid input same arity"
             (define currTupIfAtomic (list 'Node0))
             (define LHS univ)
             (define RHS Node)
             (define rightTupleContext
               (projectTupleRange currTupIfAtomic
                                  (- (node/expr-arity LHS) 1)
                                  (node/expr-arity RHS)))
             (define leftTupleContext
               (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
             (define formulas
               (list
                (node/formula/op/in
                 empty-nodeinfo
                 (list (tup2Expr leftTupleContext udt empty-nodeinfo) LHS))
                (node/formula/op/in
                 empty-nodeinfo
                 (list (tup2Expr rightTupleContext udt empty-nodeinfo) RHS))))
             (@check-equal?
              (toString (productHelper
                         (list edges Node univ) (list Node edges)
                         currTupIfAtomic empty-nodeinfo udt))
              (toString formulas)))

           (@test-case
            "TEST productHelper on valid input different arity"
            (define currTupIfAtomicDifferentArity (list 'Node0 'Node1))
            ; arity 2
            (define LHSDifferentArity edges)
            ; arity 1
            (define RHSDifferentArity Node)
            (define
              rightTupleContextDifferentArity
              (projectTupleRange
               currTupIfAtomicDifferentArity
               (- (node/expr-arity LHSDifferentArity) 1)
               (node/expr-arity RHSDifferentArity)))
            (define leftTupleContextDifferentArity
              (projectTupleRange currTupIfAtomicDifferentArity 0
                                 (node/expr-arity LHSDifferentArity)))
            (define
              formulasDifferentArity
              (list
               (node/formula/op/in empty-nodeinfo
                                   (list
                                    (tup2Expr leftTupleContextDifferentArity
                                              udt empty-nodeinfo)
                                    LHSDifferentArity))
               (node/formula/op/in empty-nodeinfo
                                   (list
                                    (tup2Expr rightTupleContextDifferentArity
                                              udt empty-nodeinfo)
                                    RHSDifferentArity))))
            (@check-equal?
             (toString
              (productHelper (list Node edges) (list Node edges)
                             currTupIfAtomicDifferentArity empty-nodeinfo udt))
             (toString formulasDifferentArity)))

           ; joinHelper


           ; tup2Expr

           (@test-case
            "TEST tup2ExprValid"
            (define tuple (list 'Node0 'Node1 'Node2))
            (@check-equal?
             (toString (tup2Expr tuple udt empty-nodeinfo))
             (toString
              (node/expr/op/-> empty-nodeinfo 3
                               (list
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
           (@test-case
            "TEST transposeTup on valid tuple"
            (define tupArity2 (list 1 2))
            (@check-equal?
             (toString (transposeTup tupArity2))
             (toString (list 2 1))))

           (@test-case
            "TEST transposeTup on validtuple arity 3"
            (define tupArity3 (list 1 2 3))
            (@check-exn
             exn:fail?
             (lambda () 
               (transposeTup tupArity3))))

           ; mustHaveTupleContext
           (@test-case
            "TEST mustHaveTupleContext on not a list error"
            (@check-exn
             exn:fail?
             (lambda () 
               (mustHaveTupleContext 'Node0 edges))))

           (@test-case
            "TEST mustHaveTupleContext on a tup with length 0"
            ; length of tup is 0
            (@check-exn
             exn:fail?
             (lambda () 
               (mustHaveTupleContext '() edges))))

           (@test-case
            "TEST mustHaveTupleContext first thing in tuple is a list"
            ; first thing in tuple is a list
            (@check-exn
             exn:fail?
             (lambda () 
               (mustHaveTupleContext (list (list 'Node0)) edges))))

           ; isGroundProduct
           (@test-case
            "TEST isGroundProduct not an expr or int constant error"
            (@check-exn
             exn:fail?
             (lambda () 
               (isGroundProduct (node/formula/op/&& empty-nodeinfo
                                                    (list 1 2))))))

           ; quantifier-var
           (@check-exn
            exn:fail?
            (lambda () 
              (isGroundProduct
               (node/expr/quantifier-var empty-nodeinfo 1 (gensym "m2q")))))

           ; return false
           (@test-case
            "TEST isGroundProduct on valid productExpr"
            (define productExpr
              (node/expr/op/-> empty-nodeinfo 2 (list Node Node)))
            (@check-equal?
             (toString (isGroundProduct productExpr))
             (toString #f)))

           ; product case, should return true
           (@test-case
            "TEST isGroundProduct quantifier case"
            (define quantifierExpr
              (node/expr/op/-> empty-nodeinfo 1
                               (list (node/expr/atom empty-nodeinfo 1 'Node0))))
            (@check-equal?
             (toString (isGroundProduct quantifierExpr))
             (toString #t)))

           ; node int constant 
           (@test-case
            "TEST isGroundProduct on valid constant"
            (@check-equal?
             (toString (isGroundProduct (node/int/constant empty-nodeinfo 1)))
             (toString #t)))

           ; atom
           (@test-case
            "TEST transposeTup on valid tuple"
            (define tupArity2 (list 1 2))
            (@check-equal?
             (toString (transposeTup tupArity2))
             (toString (list 2 1))))

           ; getColumnRight
            (@test-case
            "TEST getColumnRight error"
           (@check-exn
            exn:fail?
            (lambda () 
              (getColumnRight '()))))

           ; valid arity more than 1 (hits recursive call and base case)
           (@test-case
            "TEST getColumnRight on valid"
            (@check-equal?
             (toString (getColumnRight edges))
             (toString (join univ edges))))

           ; getColumnLeft
            (@test-case
            "TEST getColumnLeft error"
           (@check-exn
            exn:fail?
            (lambda () 
              (getColumnLeft '()))))

           ; valid arity more than 1 (hits recursive call and base case)
           (@test-case
            "TEST getColumnLeft on valid"
            (@check-equal?
             (toString (getColumnLeft edges))
             (toString (join edges univ))))

           ; createNewQuantifier
           (@test-case
            "TEST createNewQuantifier error decl does not contain both things"
            (define form (in edges edges))

            (@check-exn
             exn:fail?
             (lambda () 
               (createNewQuantifier
                (cons 1 '()) '() form udt empty-nodeinfo 'some '()))))

           (@test-case
            "TEST createNewQuantifier error desugaring unsupported"
            (define form (in edges edges))
            (@check-exn
             exn:fail?
             (lambda () 
               (createNewQuantifier (cons 1 2) '()
                                    form udt empty-nodeinfo 'no '()))))

           ; some case
           (@test-case
            "TEST createNewQuantifier on 'some' quantifier with just one decl"
            (define
              nodeBound
              (list (list 'Node0) (list 'Node1)
                    (list 'Node2) (list 'Node3) (list 'Node4) (list 'Node5)
                    (list 'Node6)))
            (define someFormula (some ([x Node]) (in x Node)))
            (define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
            (define subformulas
              (map (lambda (tup)
                     (substituteFormula (in varx Node)
                                        (list varx) varx
                                        (tup2Expr tup udt empty-nodeinfo)))
                   nodeBound))
            (@check-equal?
             (toString
              (createNewQuantifier (cons varx Node)
                                   (list varx) (in varx Node)
                                   udt empty-nodeinfo 'some someFormula))
             (toString (node/formula/op/|| empty-nodeinfo subformulas)))) 

           ; all case
           (@test-case
            "TEST createNewQuantifier on 'all' quantifier with just one decl"
            (define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
            (define someFormula (some ([x Node]) (in x Node)))
            (define allFormula (all ([x Node]) (in x Node)))
            (define
              nodeBound
              (list (list 'Node0) (list 'Node1)
                    (list 'Node2) (list 'Node3) (list 'Node4) (list 'Node5)
                    (list 'Node6)))
            (define subformulas
              (map (lambda (tup)
                     (substituteFormula (in varx Node)
                                        (list varx) varx
                                        (tup2Expr tup udt empty-nodeinfo)))
                   nodeBound))
            (@check-equal?
             (toString
              (createNewQuantifier (cons varx Node)
                                   (list varx) (in varx Node)
                                   udt empty-nodeinfo 'some someFormula))
             (toString (node/formula/op/|| empty-nodeinfo subformulas))))

           (@test-case
            "TEST createNewQuantifier on 'some' quantifier with just one decl"
            (define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
            (define someFormula (some ([x Node]) (in x Node)))
            (define allFormula (all ([x Node]) (in x Node)))
            (define
              nodeBound
              (list (list 'Node0) (list 'Node1)
                    (list 'Node2) (list 'Node3) (list 'Node4) (list 'Node5)
                    (list 'Node6)))
            (define subformulas
              (map (lambda (tup)
                     (substituteFormula (in varx Node)
                                        (list varx) varx
                                        (tup2Expr tup udt empty-nodeinfo)))
                   nodeBound))
            (@check-equal?
             (toString
              (createNewQuantifier (cons varx Node)
                                   (list varx) (in varx Node)
                                   udt empty-nodeinfo 'all allFormula))
             (toString (node/formula/op/&& empty-nodeinfo subformulas))))

           ; transitive-closure-helper
           (@test-case
            "TEST transitive-closure-helper"
            (@check-equal?
             (toString (transitiveClosureHelper edges '() 2 0 empty-nodeinfo))
             (toString (list edges (join edges edges)))))))