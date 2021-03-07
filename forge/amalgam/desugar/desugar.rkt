#lang racket ;forge/core

; Desugaring functions for Amalgam
; (full AST) -> (restricted AST without stuff like implies)
;    Note: These functions maintain an environment of
;    quantified variables to aid general functionality
; We are bringing the input into our core language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Warning: ast.rkt exports (e.g.) "and".
; This is the macro that produces an "and" formula!
; To use real Racket and, use @and

(require "../lift-bounds/lift-bounds.rkt")
(require "desugar_helpers.rkt")
(require "../substitutor/substitutor.rkt")
(provide desugarFormula desugarExpr desugarInt)

(require (prefix-in @ racket))
(require (prefix-in @ (only-in racket ->)))
(require (prefix-in forge: forge/sigs-structs)
         forge/lang/ast)
;;;;;;;;;;;;;;;;;;;;;;;;;

; This costs a great deal, but forces a noisy failure
;  Disable for "production"
(define SANITYCHECK #t)
(define DEBUG #t)

; input: formula - the current formula being desugared into simpler AST
;        quantVars - quantified variables
;        runContext - the context of the current run
;
; output: recursively creates restricted AST of the formula passed in
(define/contract (desugarFormula formula quantVars runContext)
  (@-> node/formula? list? forge:Run? (listof (or/c node/formula? symbol?)))

 ;(when DEBUG
 ;   (printf "~n---- desugarFormula called with: ~a~n" formula))

  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant info type)
     (cond
       [(and (equal? (node/formula/constant-type formula) 'true))
        (list true 'constantFormula)]
       [(and (equal? (node/formula/constant-type formula) 'false))
        (list false 'constantFormula)]
       [else (list formula 'constantFormula)])]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     (desugarFormulaOp formula quantVars args runContext info)]
    
    ; multiplicity formula (some, one, ...)
    ; desugar some e to quantified fmla some x_fresh : union(upperbound(e)) | x_fresh in e
    [(node/formula/multiplicity info mult expr)
     (define arity (node/expr-arity expr))
     (define newDecls
       (build-list arity (lambda (i)
                           (define currColExpr (getGivenColumn expr i 0 arity))
                           (define uppers (liftBoundsExpr currColExpr quantVars runContext))
                           (define union-of-bounds
                             (+/info info (map (lambda (tup)
                                                 (tup2Expr tup runContext info)) uppers))) 
                           (cons (node/expr/quantifier-var info 1 (gensym "m2q") 'm2q) union-of-bounds))))
     (define productOfFreshVars (->/info info (map car newDecls)))
     (define newFormula (in/info info (list productOfFreshVars expr)))
     (list (node/formula/quantified info mult newDecls newFormula) 'multiplicityFormula)]
      
    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified info quantifier decls subForm)
     ; In the case where the quantifier is not a 'some or 'all, desugar into
     ; somes/alls

     (cond [(not (or (equal? quantifier 'some)
                     (equal? quantifier 'all)))
            (cond
              ; no x: A | r.x in q ------> all x: A | not (r.x in q)
                
              [(equal? quantifier 'no)
               (define negatedFormula (!/info info (list subForm)))
               (list
                (node/formula/quantified info 'all decls negatedFormula)
                'noQuantifierFormula)]

              ; one x: A | r.x in q ------>
              ;    (some x: A | r.x in q and (all y: A-x | not (r.x in q)))
              [(equal? quantifier 'one)
               (define negatedFormula (!/info info (list subForm)))
               (define subtractedDecls
                 (map (lambda (d)
                        ;(printf "in map d ~a" d)
                        (cons
                         (node/expr/quantifier-var info
                                                   (node/expr-arity (cdr d))
                                                   (gensym "quantiOne")
                                                   'quantiOne)
                         (-/info info (list (cdr d) (car d))))) decls))               
               
               (define newQuantFormRHS
                 (node/formula/quantified info 'all subtractedDecls negatedFormula))
               
               ; Put LHS and RHS together 
               (define desugaredAnd
                 (&&/info info (list subForm newQuantFormRHS)))
               (list
                (node/formula/quantified info 'some decls desugaredAnd)
                'oneQuantifierFormula)]

              ; lone x: A | r.x in q ------>
              ;   (no x: A | r.x in q) or (one x: A | r.x in q)
              [(equal? quantifier 'lone)
               (define newQuantFormLHS
                 (node/formula/quantified info 'no decls subForm))
               (define newQuantFormRHS
                 (node/formula/quantified info 'one decls subForm))
               (list
                (||/info info (list newQuantFormLHS newQuantFormRHS))
                'loneQuantifierFormula)])]
           
           [else
            (list (createNewQuant decls quantVars subForm runContext info quantifier) 'quantifiedFormula)])]))
  
; Debug mode will evaluate the formula in the latest instance produced by runContext
;   expecting the same result (modulo currSign)
; NOTE WELL: this is always with respect to the latest instance.
;  If we fix the evaluator to work with arbitrary instances, we'll need to adapt this to take an instance or #f.  
;  (when SANITYCHECK
;    (unless (equal? (if currSign
;                        (evaluate runContext 'unused formula)
;                        (not (evaluate runContext 'unused formula)))
;                    (evaluate runContext 'unused resultFormula))
;      (error (format "desugarFormula would have produced a formula (sign=~a) with a different meaning in the latest instance.~nCalled with:~a~nProduced: ~a~n"
;                     currSign formula resultFormula))))

; input: formula - the current formula being desugared into simpler AST
;        quantVars - quantified variables
;        runContext - the context of the current run
;        args - a list of the arguments of the current formula
;        currTupIfAtomic - the tuple containing the implicit LHS of the current
;                          "in"
;        info - the info of the original node
;
; output: This function is recursively calling every element in args and pass
; it to the original recursive function. 
(define/contract (desugarFormulaOp formula quantVars args
                                   runContext info)
  (@-> node/formula? list?
       (or/c (listof node/formula?) (listof node/expr?) (listof symbol?))
       forge:Run?       
       nodeinfo? (listof (or/c node/formula? symbol?)))
  (match formula
    
    ; IMPLIES
    [(? node/formula/op/=>?)
     ; The desugared version of IMPLIES is: (not LHS) OR (RHS)
     (define ante (!/info info (list (first args))))
     (define conseq (second args))
     (list (||/info info (list ante conseq)) 'impliesFormula)]

    ; IN (atomic fmla)
    ; This function has two cases, the ground case and the case where we build
    ; an and-of-implications.
    ; Some examples can be seen below: 
    ;     Node0->Node1 in ^ edges   <--- this is a ground case of IN! we know
    ;     the current tuplen Node0->Node1 + Node1->Node2 in ^edges <--- need to
    ;     turn into an and-of-implications edges in ~edges <--- same deal, need
    ;     to build an and-of-implications
    [(? node/formula/op/in?)
     (define leftE (first args))
     (define rightE (second args))
     
     ; We don't yet know which relation's bounds will be needed, so just pass
     ; them all in
     (define liftedUpperBounds (liftBoundsExpr  leftE '() runContext))
     (cond
       [(and (isGroundProduct leftE) (equal? (length liftedUpperBounds) 1))
        ; ground case. we have a currentTuple now, and want to desugar the RHS
        (desugarExpr rightE quantVars
                     (first liftedUpperBounds) runContext)]
       [else
        ; build a big "and" of: for every tuple T in liftedUpperBounds:
        ; (T in leftE) implies (T in rightE)
        (define desugaredAnd
          (&&/info info (map (lambda (x)
                               (define tupExpr
                                 (tup2Expr x runContext info))
                               (define LHS
                                 (in/info info (list tupExpr leftE)))
                               (define RHS
                                 (in/info info (list tupExpr rightE)))
                               (=>/info info (list LHS RHS)))
                             liftedUpperBounds)))
        (list desugaredAnd 'ungroundINFormula)])]

    ; EQUALS 
    [(? node/formula/op/=?)
     ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
     (define LHS (in/info info (list (first args) (second args))))
     (define RHS (in/info info (list (second args) (first args))))
     (list (&&/info info (list LHS RHS)) 'equalsFormula)] 

    ; INTEGER >
    [(? node/formula/op/int>?)
     (error "amalgam: int > not supported ~n")]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (error "amalgam: int < not supported ~n")]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (error "amalgam: int = not supported ~n")]))

; input: expr - the current expression being desugared into simpler AST
;        quantVars - quantified variables
;        runContext - the context of the current run
;        args - a list of the arguments of the current expression
;        currTupIfAtomic - the tuple containing the implicit LHS of the current
;                          "in"
;
; output: This function is desugaring the current expression (returns simpler
;         result of desugared expression)
(define/contract (desugarExpr expr quantVars currTupIfAtomic runContext)
  ; Error message to check that we are only taking in expressions
  (@-> node/expr? list? (listof symbol?) forge:Run? (listof (or/c node/formula? symbol?)))

  ;(when DEBUG
  ;  (printf "~n---- desugarExpr called (tuple=~a) with: ~a~n" currTupIfAtomic expr))
  
  
  ; Should always have a currTupIfAtomic when calling
  (mustHaveTupleContext currTupIfAtomic expr)
  (when (not (equal? (length currTupIfAtomic) (node/expr-arity expr)))
    (error (format "desugarExpr: current tuple had different arity: ~a vs ~a" currTupIfAtomic expr)))

  (match expr
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent isVar)
     (list
      (in/info info (list (tup2Expr currTupIfAtomic runContext info) expr))
      'relationNameExpr)]

    ; atom (base case)
    [(node/expr/atom info arity name)
     (list
      (in/info info (list (tup2Expr currTupIfAtomic runContext info) expr))
      'atomExpr)]

    ; The Int constant
    [(node/expr/constant info 1 'Int)
     (list
      (in/info info (list (tup2Expr currTupIfAtomic runContext info) expr))
      'intConstantExpr)]

    ; other expression constants
    [(node/expr/constant info arity type)
     (list
      (in/info info (list (tup2Expr currTupIfAtomic runContext info) expr))
      'otherExprConstantExpr)]

    ; quantified variable (depends on scope!)
    [(node/expr/quantifier-var info arity sym name)     
     (error
      "amalgam: A quantified variable was passed into desugarExpr. ~n")]

    ; ^ base cases
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (desugarExprOp expr quantVars args currTupIfAtomic runContext info)]
    
    ; set comprehension e.g. {n : Node | some n.edges}
    ; t in {x0: A0, x1: A1, ... | fmla } means:
    ;   t0 in A0 and t0 in A1 and ... fmla[t0/x0, t1/x1, ...] 
    [(node/expr/comprehension info len decls form)
     ; account for multiple variables
     (define vars (map car decls))
     (let ([quantVars (append vars quantVars)])
       ;  t0 in A0 ...
       (define LHSSubformula (setComprehensionAndHelper currTupIfAtomic
                                                        decls info runContext))

       ; fmla[t0/x0, t1/x1, ...]
       (define RHSSubformula
         (setComprehensionSubHelper form currTupIfAtomic quantVars decls
                                    runContext info))
       ; Put both formulas together
       (list
        (&&/info info (append LHSSubformula (list RHSSubformula)))
        'setComprehensionExpr))]))

; input: expr - the current expression being desugared into simpler AST
;        quantVars - quantified variables
;        runContext - the context of the current run
;        args - a list of the arguments of the current expression
;        currTupIfAtomic - the tuple containing the implicit LHS of the current
;                          "in"
;        info - the info of the original node
;
; output: This function is desugaring the current expression (returns simpler
;         result of desugared expression)
(define/contract (desugarExprOp  expr quantVars args
                                 currTupIfAtomic runContext info)
  (@-> node/expr? list? (listof node/expr?) (listof symbol?)
       forge:Run? nodeinfo? (listof (or/c node/formula? symbol?)))
  (mustHaveTupleContext currTupIfAtomic expr)
  (match expr

    ; UNION
    ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR
    ; (currTupIfAtomic in RHS)
    [(? node/expr/op/+?)
     (define desugaredChildren
       (map
        (lambda (child)
          (first (desugarExpr child quantVars currTupIfAtomic runContext)))
        args))
     (list (||/info info desugaredChildren) 'unionExpr)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (cond
       [(not (equal? (length args) 2))
        (error("Setminus should not be given more than two arguments ~n"))]
       [else
        ; The desugared version of SETMINUS is: (currTupIfAtomic in LHS) and
        ;  (not(currTupIfAtomic in RHS))
        (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext info))
        (define LHS (in/info info (list currTupIfAtomicExpr (first args))))
        (define RHS (!/info info (list (in/info info
                                                (list currTupIfAtomicExpr
                                                      (second args))))))
        ; Create the final desugared version of SETMINUS by joining LHS and RHS
        ; with an AND 
        (list (&&/info info (list LHS RHS)) 'setMinusExpr)])]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child)
          (first (desugarExpr child quantVars currTupIfAtomic runContext)))
        args))
     ; The desugared version of INTERSECTION is: (currTupIfAtomic in CHILD)
     ; AND (currTupIfAtomic in CHILD)
     (list (&&/info info desugaredChildren) 'intersectionExpr)]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (cond
       [(@> (length args) 2)
        (define newLHS (first args))
        (define newRHS (->/info info (rest args)))
        (define newProduct (->/info info (list newLHS newRHS)))
        (desugarExpr newProduct quantVars currTupIfAtomic runContext)]

       ; we are in the binary case
       [(@= (length args) 2)            
        (list
         (&&/info info (productHelper (first args) (second args)
                                     currTupIfAtomic info runContext))
         'binaryProductExpr)]
       
       [(equal? (node/expr-arity expr) 1)
        ; if arity is 1, only call desugarExpr on the first thing
        (desugarExpr (first args)
                     quantVars currTupIfAtomic runContext)]
       [else (error
              (format "Expression ~a in product had arity less than 1" expr))])]
    
    ; JOIN
    [(? node/expr/op/join?)
     (cond
       [(@> (length args) 2)
        (define len (length args))
        (define newLHS (first args))
        (define arityRHS (apply join-arity (map (lambda (x) (node/expr-arity x))
                                                (rest args))))
        (define newRHS (join/info info (rest args)))
        (define newJoin (join/info info (list newLHS newRHS)))
        (desugarExpr newJoin quantVars currTupIfAtomic runContext)]
       [(equal? (length args) 2)

        (unless (equal? (length currTupIfAtomic) (node/expr-arity (first args)))
          (error "The arity of the LHS doesn't match the size of currTupIfAtomic"))

        (define UBA (liftBoundsExpr (first args) quantVars runContext))
        (define UBB (liftBoundsExpr (second args) quantVars runContext))
        (define allPairs (cartesian-product UBA UBB))
        (define newArgs
          (filter-map
           (lambda (lstpr)
             (define-values (ta tb) (values (first lstpr) (second lstpr)))
             
             (define taExpr (tup2Expr ta runContext info))
             (define tbExpr (tup2Expr tb runContext info))
             (cond
               [(equal? (joinTupleDesugar ta tb) currTupIfAtomic)
                (&&/info info (list
                               (in/info info (list taExpr (first args)))
                               (in/info info (list tbExpr (second args)))))]
               [else #f]))
           allPairs))
        (list (||/info info newArgs) 'joinExpr)]
       [else
        (error (format "Expression ~a in join had arity less than 1" expr))])]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     ; Write transitive closure case 
     ;^e = e + e.e + e.e.e ... up to firstCol(e)
     ; #dots = #(UB(leftCol)+UB(rightCol)) - 1  ?
     ; #times-e-is-used-in-biggest-join = #(UB(leftCol)+UB(rightCol))
     
     ; get the upper bound to be called in extendResult
     (define uppers (liftBoundsExpr expr quantVars runContext))

     ; build paths starting from LHS of current tuple
     (define extendResult (extendPossiblePaths uppers (list
                                                       (first currTupIfAtomic))))
     ; Check the endpoint and remove items that do not match
     (define endPoint (last currTupIfAtomic))

     ; A list containing *paths* from (first currentTuple) to (second currentTuple)
     (define filteredExtendResult
       (filter
        (lambda (result) (equal? (last result) endPoint)) extendResult))
     
     ; Go through everything in filteredExtendResult to create big AND
     ; by calling helper
     (cond
       [(empty? filteredExtendResult) (list (||/info info #f) 'transitiveClosureExpr)]
       [else 
        (define resultingAnd
          (map (lambda (path)
                 (transitiveClosureAnd
                  path (first args) info runContext '())) filteredExtendResult))
        (list (||/info info resultingAnd) 'transitiveClosureExpr)])]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (define transitiveClosure (^/info info (list (first args))))
     (define desugaredRClosure
       (+/info info (list iden transitiveClosure)))
     (define inFormula
       (in/info info (list (tup2Expr currTupIfAtomic runContext info)
                           desugaredRClosure)))
     (list inFormula 'reflexiveTCExpr)]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (define transposedCurrTupIfAtomic (transposeTup currTupIfAtomic))
     (desugarExpr
      (first args) quantVars transposedCurrTupIfAtomic runContext)]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (error "amalgam: singleton is not supported ~n")]))

; input: expr - the current expression being desugared into simpler AST
;        quantVars - quantified variables
;        runContext - the context of the current run
;
; output: amalgam does not support integer operations, so this
;         returns an error
(define (desugarInt expr quantVars runContext)
  (match expr
    ; CONSTANT INT
    [(node/int/constant info value)
     (error "amalgam: constant int not supported ~n")]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)
     (desugarIntOp expr quantVars args runContext)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age
    [(node/int/sum-quant info decls intExpr)
     (error "amalgam: sum quantifier not supported ~n")]))

; input: expr - the current expression being desugared into simpler AST
;        quantVars - quantified variables
;        runContext - the context of the current run
;        args - a list of the arguments of the current formula
;
; output: amalgam does not support integer operations, so this returns an error
(define (desugarIntOp expr quantVars args runContext)
  (match expr
    ; int addition
    [(? node/int/op/add?)
     (error "amalgam: int + not supported~n")]
    
    ; int subtraction
    [(? node/int/op/subtract?)
     (error "amalgam: int - not supported~n")]
    
    ; int multiplication
    [(? node/int/op/multiply?)
     (error "amalgam: int * not supported~n")]
    
    ; int division
    [(? node/int/op/divide?)
     (error "amalgam: int / not supported ~n")]
    
    ; int sum (also used as typecasting from relation to int)
    ; e.g. {1} --> 1 or {1, 2} --> 3
    [(? node/int/op/sum?)
     (error "amalgam: sum not supported ~n")]
    
    ; cardinality (e.g., #Node)
    [(? node/int/op/card?)
     (error "amalgam: cardinality not supported ~n")]
    
    ; remainder/modulo
    [(? node/int/op/remainder?)
     (error "amalgam: int % (modulo) not supported~n")]
    
    ; absolute value
    [(? node/int/op/abs?)
     (error "amalgam: int abs not supported~n")]
    
    ; signOf 
    [(? node/int/op/sign?)
     (error "amalgam: int signOf not supported~n")]))



