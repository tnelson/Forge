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
            (@test-case
             "TEST AND formula currSign true"
             (define andTest (and true false))
             (@check-equal?
              (toString (desugarFormula andTest '() udt #t))
              (toString (node/formula/op/&& empty-nodeinfo (list true false)))))

            ; Consider De Morgan's Laws: !(a && b)  -----> !a || !b
            ; not true and false     (#t sign)
            ; true and false    (#f sign)  <--- this case
            ; <> or <>
            ; not true or not false
            (@test-case
             "TEST AND formula currSign false"
             (define andTest (and true false))
             (@check-equal?
              (toString (desugarFormula andTest '() udt #f))
              (toString (node/formula/op/|| empty-nodeinfo (list false true)))))

            ; OR
            (@test-case
             "TEST ORR formula currSign true"
             (define orTest (or true false))
             (@check-equal?
              (toString (desugarFormula orTest '() udt #t))
              (toString (node/formula/op/|| empty-nodeinfo (list true false)))))

            (@test-case
             "TEST OR formula currSign false"
             (define orTest (or true false))
             (@check-equal?
              (toString (desugarFormula orTest '() udt #f))
              (toString (node/formula/op/&& empty-nodeinfo (list false true)))))

            ; IMPLIES
            (@test-case
             "TEST implies formula"
             (define impliesTest (implies true false))
             (@check-equal?
              (toString (desugarFormula impliesTest '() udt #t))
              ;desugars to (not LHS) OR (RHS) 
              (toString (node/formula/op/|| empty-nodeinfo (list false false)))))

            ; IN For Not Ground LeftE
            (@test-case
             "TEST In formula"
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
        


             (@check-equal?
              (toString (desugarFormula inTest '() udt #t))
              (toString (desugarFormula desugaredAnd '() udt #t))))

            ; IN for ground LeftE

            ; EQUALS
            ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
            (@test-case
             "TEST EQUALS formula"
             (define equalsTest (= Node Node))
             (define LHS (node/formula/op/in empty-nodeinfo (list Node Node)))
             (define RHS (node/formula/op/in empty-nodeinfo (list Node Node)))
             (define desugaredEquals (node/formula/op/&& empty-nodeinfo (list LHS RHS)))
             (@check-equal?
              (toString (desugarFormula equalsTest '() udt #t))
              (toString (desugarFormula  desugaredEquals '() udt #t))))

            ; NEGATION
            ; QUESTION: SHOULD THIS BE CALLING DESUGARFORMULAOP INSTEAD?
            (@test-case
             "TEST NEGATION formula"
             (define negationTest (! true))
             (@check-equal?
              (toString (desugarFormula negationTest '() udt #t))
              ;desugars to (not LHS) OR (RHS) 
              (toString false)))

            ; INTEGER <
            (@check-exn
             exn:fail?
             (lambda ()
               (define varIntConstX (node/int/constant empty-nodeinfo 1))
               (define varIntConstY  (node/int/constant empty-nodeinfo 2))
               (define intLess (< varIntConstX varIntConstY ))
               (desugarFormula intLess '() udt #t)))

            ; INTEGER >
            (@check-exn
             exn:fail?

             (lambda ()
               (define varIntConstX (node/int/constant empty-nodeinfo 1))
               (define varIntConstY  (node/int/constant empty-nodeinfo 2))
               (define intGreater (> varIntConstX varIntConstY ))
               (desugarFormula intGreater '() udt #t)))

            ; INTEGER =
            (@check-exn
             exn:fail?

             (lambda ()
               (define varIntConstX (node/int/constant empty-nodeinfo 1))
               (define varIntConstY  (node/int/constant empty-nodeinfo 2))
               (define intEquals (= varIntConstX varIntConstY ))
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
            (@test-case
             "TEST UNION expression"
             (define unionTest (+ Node Node))
             (define desugaredChild (desugarExpr Node '() '(Node) udt #t))
             (define desugaredWithOr (node/formula/op/|| empty-nodeinfo (list desugaredChild desugaredChild)))
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

            ; REFLEXIVE-TRANSITIVE CLOSURE

            ; TRANSPOSE

            ; SINGLETON

            ;;;;;;;;;;;;;;;;;; DESUGAR INT ;;;;;;;;;;;;;;;;;;;;

            ; CONSTANT INT

            ; sum
            (@check-exn
             exn:fail?
             (lambda ()
               (define intSum (sum edges))

               (desugarInt intSum '() udt)))

            ; desugarIntOp
            ; int addition
            (@check-exn

             exn:fail?
             (lambda ()
               (define intAdd (+ 1 2))
               (desugarInt intAdd '() udt #t)))

            ; int subtraction
            (@check-exn

             exn:fail?
             (lambda ()
               (define intSub (- 1 2))
               (desugarInt intSub '() udt #t)))

            ; int division
            (@check-exn
             exn:fail?
             (lambda ()
               (define intDiv (/ 1 2))

               (desugarInt intDiv '() udt #t)))

            ; int mult
            (@check-exn
             exn:fail?
             (lambda ()
               (define intMult (* 1 2))

               (desugarInt intMult '() udt #t)))

            ; int sum
            (@check-exn

             exn:fail?
             (lambda ()
               (define intSumErr (sum Node))
               (desugarInt intSumErr '() udt #t)))

            ; int card
            (@check-exn

             exn:fail?
             (lambda ()
               (define intCard (card Node))
               (desugarInt intCard '() udt #t)))

            ; int mod
            (@check-exn

             exn:fail?
             (lambda ()
               (define intMod (modulo 0 5))
               (desugarInt intMod '() udt #t)))

            ; int abs
            ; TODO: finish this case
            #|(define intAbs (absolute 1))
(@check-exn
 exn:fail?
 (lambda () 
   (desugarInt intAbs '() udt #t)))|#

            ; int sign-of
            (@check-exn

             exn:fail?
             (lambda ()
               (define intSignOf (sgn 1))
               (desugarInt intSignOf '() udt #t)))))