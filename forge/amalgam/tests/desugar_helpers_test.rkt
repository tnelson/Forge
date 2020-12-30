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

          (@test-case
           "TEST setComprehensionAndHelper test"
           (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
           (@check-equal?
            (setComprehensionAndHelper (list 'Node0) (list (cons x Node))
                                                 empty-nodeinfo udt)
            (list (in (list (tup2Expr (list 'Node0) udt empty-nodeinfo) Node)))))

           (@test-case
           "TEST setComprehensionSubHelper test"
           (define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
           (define y (node/expr/quantifier-var empty-nodeinfo 1 'y))
           (define relRes (rel '(Node) 'univ "Node"))
           (@check-equal?
            (setComprehensionSubHelper
                       (in x Node) (list 'Node1) '(x) (list (cons x Node))
                       udt empty-nodeinfo)
            (in (list (tup2Expr (list 'Node1) udt empty-nodeinfo) relRes))))

          (@test-case
             "TEST extendPossiblePaths on empty list"
             (@check-equal?
              (extendPossiblePaths '() '(1))
              '()))

            (@test-case
             "TEST extendPossiblePaths on length 1 list"
             (@check-equal?
              (extendPossiblePaths '((1 2)) '(1))
              '((1 2))))

            (@test-case
             "TEST extendPossiblePaths on valid list"
             (@check-equal?
              (extendPossiblePaths '((1 2) (2 3) (1 5) (4 7) (7 8)) '(1))
              '((1 2) (1 5) (1 2 3))))

            (@test-case
             "TEST extendPossiblePaths on valid list with cycle"
             (@check-equal?
              (extendPossiblePaths
                '((1 2) (2 3) (1 5) (4 7) (7 8) (2 5) (2 2)) '(1))
              '((1 2) (1 5) (1 2 3) (1 2 5) (1 2 2))))

            (@test-case
             "TEST transitiveClosureIn on valid input"
             (@check-equal?
              (transitiveClosureIn 'Node0 'Node1 edges
                                    empty-nodeinfo udt)
              (in
                (list (->/info
                       empty-nodeinfo
                       (list (node/expr/atom empty-nodeinfo 1 'Node0)
                             (node/expr/atom empty-nodeinfo 1 'Node1)))
                      edges))))

            (@test-case
             "TEST productHelper on valid input same arity"
             (define currTupIfAtomic (list 'Node0 'Node1))
             (define univRel (rel '(Node) 'univ "Node"))
             (define LHS univ)
             (define RHS Node)
             (define rightTupleContext
               (projectTupleRange currTupIfAtomic (node/expr-arity LHS)
                                  (length currTupIfAtomic)))
             (define leftTupleContext
               (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
             (define formulas
               (list
                (in
                 (list (tup2Expr leftTupleContext udt empty-nodeinfo) univRel))
                (in
                 (list (tup2Expr rightTupleContext udt empty-nodeinfo) univRel))))
             (@check-equal?
              (productHelper Node Node currTupIfAtomic empty-nodeinfo udt)
              formulas))

           (@test-case
            "TEST tup2ExprValid"
            (define tuple (list 'Node0 'Node1 'Node2))
            (@check-equal?
             (tup2Expr tuple udt empty-nodeinfo)
             (->/info empty-nodeinfo 
                               (list
                                (node/expr/atom empty-nodeinfo 1 'Node0)
                                (node/expr/atom empty-nodeinfo 1 'Node1)
                                (node/expr/atom empty-nodeinfo 1 'Node2)))))

           (@test-case
            "TEST tup2ExprError"
            (@check-exn
             exn:fail?
             (lambda () 
               (tup2Expr (list (list 'Node0)) udt empty-nodeinfo))))

           (@test-case
            "TEST transposeTup on valid tuple"
            (define tupArity2 (list 1 2))
            (@check-equal?
             (transposeTup tupArity2)
             (list 2 1)))

           (@test-case
            "TEST transposeTup on validtuple arity 3"
            (define tupArity3 (list 1 2 3))
            (@check-exn
             exn:fail?
             (lambda () 
               (transposeTup tupArity3))))

           (@test-case
            "TEST mustHaveTupleContext on not a list error"
            (@check-exn
             exn:fail?
             (lambda () 
               (mustHaveTupleContext 'Node0 edges))))

           (@test-case
            "TEST mustHaveTupleContext on a tup with length 0"
            (@check-exn
             exn:fail?
             (lambda () 
               (mustHaveTupleContext '() edges))))

           (@test-case
            "TEST mustHaveTupleContext first thing in tuple is a list"
            (@check-exn
             exn:fail?
             (lambda () 
               (mustHaveTupleContext (list (list 'Node0)) edges))))

           (@test-case
            "TEST isGroundProduct not an expr or int constant error"
            (@check-exn
             exn:fail?
             (lambda () 
               (isGroundProduct (&& (list 1 2))))))

           (@check-exn
            exn:fail?
            (lambda () 
              (isGroundProduct
               (node/expr/quantifier-var empty-nodeinfo 1 (gensym "m2q")))))

           (@test-case
            "TEST isGroundProduct on valid productExpr"
            (define productExpr
              (->/info empty-nodeinfo (list Node Node)))
            (@check-equal?
             (isGroundProduct productExpr)
             #f))

           (@test-case
            "TEST isGroundProduct quantifier case"
            (define quantifierExpr
              (node/expr/op/-> empty-nodeinfo 1
                               (list (node/expr/atom empty-nodeinfo 1 'Node0))))
            (@check-equal?
             (isGroundProduct quantifierExpr)
             #t))

           (@test-case
            "TEST isGroundProduct on valid constant"
            (@check-equal?
             (isGroundProduct (node/int/constant empty-nodeinfo 1))
             #t))

           (@test-case
            "TEST transposeTup on valid tuple"
            (define tupArity2 (list 1 2))
            (@check-equal?
             (transposeTup tupArity2)
             (list 2 1)))

            (@test-case
            "TEST getColumnRight error"
           (@check-exn
            exn:fail?
            (lambda () 
              (getColumnRight '()))))

           (@test-case
            "TEST getColumnRight on valid"
            (@check-equal?
             (getColumnRight edges)
             (join univ edges)))

           (@test-case
            "TEST getGivenColum on arity 1"
            (@check-equal?
             (getGivenColumn Node 0 0 1)
             (rel '(Node) 'univ "Node")))

           (@test-case
            "TEST getGivenColumn on arity 2"
            (@check-equal?
             (getGivenColumn edges 1 0 2)
             (join univ edges)))

           (@test-case
            "TEST getGivenColumn on arity 2 part 2"
            (@check-equal?
             (getGivenColumn edges 0 0 2)
             (join edges univ)))

           (@test-case
            "TEST getGivenColumn on arity 3"
            (@check-equal?
             (getGivenColumn (-> Node Node Node) 1 0 3)
             (join (join univ (-> Node Node Node)) univ)))

           (@test-case
            "TEST getGivenColumn on arity 4"
            (@check-equal?
             (getGivenColumn (-> edges edges) 1 0 4)
             (join (join (join univ (-> edges edges)) univ) univ)))

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
             (createNewQuantifier (cons varx Node)
                                   (list varx) (in varx Node)
                                   udt empty-nodeinfo 'some someFormula)
             (|| subformulas)))

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
             (createNewQuantifier (cons varx Node)
                                   (list varx) (in varx Node)
                                   udt empty-nodeinfo 'some someFormula)
             (|| subformulas)))

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
             (createNewQuantifier (cons varx Node)
                                   (list varx) (in varx Node)
                                   udt empty-nodeinfo 'all allFormula)
             (&& subformulas)))))