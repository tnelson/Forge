#lang forge/core

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
(provide desugarFormula)
(require debug/repl)
(require (prefix-in @ racket))
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugarFormula formula quantVars runContext currSign)
  (printf "desugarFormula~n")
  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant info type)
     (define formString (toStringSub formula))
     (cond
       [(and (equal? formString (toStringSub 'true)) currSign) true]
       [(and (equal? formString (toStringSub 'false)) currSign) false]
       [(and (equal? formString (toStringSub 'true)) (not currSign)) false]
       [(and (equal? formString (toStringSub 'false)) (not currSign)) true]
       [else formula])]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     ; We want to pass in the currTupIfAtomic as the implicit LHS
     (desugarFormulaOp
      formula quantVars args runContext currSign (first args) info)]
    
    ; multiplicity formula (some, one, ...)
    ; desugar some e to quantified fmla some x_fresh : union(upperbound(e)) | x_fresh in e
    [(node/formula/multiplicity info mult expr)
     (define freshVar (node/expr/quantifier-var info 1 (gensym "m2q")))
     (define uppers (liftBoundsExpr  expr quantVars runContext))
     (define unionOfBounds
       (node/expr/op/+ info
                       (node/expr-arity
                        (tup2Expr (first uppers) runContext info))

                       (map (lambda (tup)
                              (tup2Expr tup runContext info)) uppers)))
     (define domain unionOfBounds) 
     (define newFormula (node/formula/op/in info (list freshVar expr)))
     (define newDecls (list (cons freshVar domain)))
     (define desugaredMultiplicity
       (node/formula/quantified info mult newDecls newFormula))
     (desugarFormula desugaredMultiplicity quantVars runContext currSign)]
    

    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified info quantifier decls subForm)
     ; In the case where the quantifier is not a 'some or 'all, desugar into
     ; somes/alls  
     (cond [(not (or (equal? quantifier 'some)
                     (equal? quantifier 'all)))
            (cond
              ; no x: A | r.x in q ------> all x: A | not (r.x in q)
              [(equal? quantifier 'no)
               (define negatedFormula (node/formula/op/! info (list subForm)))
               (define newQuantFormula
                 (node/formula/quantified info 'all decls negatedFormula))
               (desugarFormula newQuantFormula quantVars runContext currSign)]

              ; one x: A | r.x in q ------>
              ;    (some x: A | r.x in q and (all y: A-x | not (r.x in q)))
              [(equal? quantifier 'one)
               ; This is making the RHS 
               (define negatedFormula (node/formula/op/! info (list subForm)))
               
               (define subtractedDecls
                 (node/expr/op/- info (node/expr-arity (cdr (car decls)))
                                 (list (cdr (car decls)) (car (car decls)))))
               (define quantifiedVarOne
                 (node/expr/quantifier-var info 1 (gensym "quantiOne")))
               (define newDecls (list (cons quantifiedVarOne subtractedDecls)))
               (define newQuantFormRHS
                 (node/formula/quantified info 'all newDecls negatedFormula))
               
               ; Put LHS and RHS together 
               (define desugaredAnd
                 (node/formula/op/&& info
                                     (list subForm newQuantFormRHS)))
      
               (define newQuantFormLHS
                 (node/formula/quantified info 'some decls desugaredAnd))
                        
               (desugarFormula newQuantFormLHS quantVars runContext currSign)]

              ; lone x: A | r.x in q ------>
              ;   (no x: A | r.x in q) or (one x: A | r.x in q)
              [(equal? quantifier 'lone)
               (define newQuantFormLHS
                 (node/formula/quantified info 'no decls subForm))
               (define newQuantFormRHS
                 (node/formula/quantified info 'one decls subForm))
               (define desugaredOR
                 (node/formula/op/|| info
                                     (list newQuantFormLHS newQuantFormRHS)))
               (desugarFormula desugaredOR quantVars runContext currSign)])]

           ; if it's got multiple variables, foldl over the helper that gets
           ; big AND or OR of subformulas 
           [(not (equal? 1 (length decls)))
 
            (when (and (not (equal? (quantifier 'and)))
                       (not (equal? (quantifier 'some))))
              (error
               (format
                "Multiple quantifiers with something other than all/some: ~a"
                subForm)))
            (define currQuantifier
              (list (createNewQuantifier
                     (first decls) quantVars subForm runContext
                     info quantifier formula)))
            (define quants
              (foldl (lambda (curr acc)
                       (append (createNewQuantifier curr quantVars subForm
                                                    runContext info quantifier
                                                    formula) acc))
                     currQuantifier (rest decls)))
            (define unionOfQuants (node/formula/op/|| info quants))
            (desugarFormula unionOfQuants quantVars runContext currSign)]

           [else
            (define newFormula (createNewQuantifier (first decls)
                                                    quantVars subForm runContext
                                                    info quantifier formula))
            (desugarFormula newFormula quantVars runContext currSign)])]

    ; truth and falsity
    [#t  (printf "desugar true~n")]
    [#f (printf "desugar false~n")]
    ))

; This function is recursively calling every element in args and pass it to the
; original recursive function. 
(define (desugarFormulaOp formula quantVars args
                          runContext currSign currTupIfAtomic info)
  (printf "desugarFormulaOp~n")
  (match formula

    ; AND 
    [(? node/formula/op/&&?) 
     (define desugaredArgs
       (map (lambda (x) (desugarFormula x quantVars runContext currSign)) args))
     (cond
       [currSign (node/formula/op/&& info desugaredArgs)]
       [else (node/formula/op/|| info desugaredArgs)])]

    ; OR
    [(? node/formula/op/||?)
     (define desugaredArgs
       (map (lambda (x) (desugarFormula x quantVars runContext currSign)) args))
     (cond
       [currSign (node/formula/op/|| info desugaredArgs)]
       [else (node/formula/op/&& info desugaredArgs)])]

    ; IMPLIES
    [(? node/formula/op/=>?)
     ; The desugared version of IMPLIES is: (not LHS) OR (RHS)
     (define ante (node/formula/op/! info (list (first args))))
     (define conseq (second args))
     (define desugaredImplies (node/formula/op/|| info (list ante conseq)))
     (desugarFormula desugaredImplies quantVars runContext currSign)]

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
        (printf "entering ground case: ~a ~a" rightE (first liftedUpperBounds))
        (desugarExpr rightE quantVars
                     (first liftedUpperBounds) runContext currSign)]
       [else
        ; build a big "and" of: for every tuple T in liftedUpperBounds:
        ; (T in leftE) implies (T in rightE)
        (define desugaredAnd
          (node/formula/op/&& info
                              (map (lambda (x)
                                     (define tupExpr
                                       (tup2Expr x runContext info))
                                     (define LHS
                                       (node/formula/op/in info
                                                           (list tupExpr leftE)))
                                     (define RHS
                                       (node/formula/op/in info
                                                           (list tupExpr rightE)))
                                     (node/formula/op/=> info
                                                         (list LHS RHS)))
                                   liftedUpperBounds))) 
        (desugarFormula desugaredAnd quantVars runContext currSign)])]

    ; EQUALS 
    [(? node/formula/op/=?)
     ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
     (define LHS (node/formula/op/in info (list (first args) (second args))))
     (define RHS (node/formula/op/in info (list (second args) (first args))))
     (define desugaredEquals (node/formula/op/&& info (list LHS RHS)))
     (desugarFormula desugaredEquals quantVars runContext currSign)]

    ; NEGATION
    [(? node/formula/op/!?)
     ; The desugared version of NEGATION is to flip the currSign type
     (desugarFormula (first args) quantVars runContext (not currSign))]   

    ; INTEGER >
    [(? node/formula/op/int>?)
     (error "amalgam: int > not supported ~n")]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (error "amalgam: int < not supported ~n")]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (error "amalgam: int = not supported ~n")]))

(define (desugarExpr expr quantVars currTupIfAtomic runContext currSign)
  (printf "desugarExpr~n")
  ; Error message to check that we are only taking in expressions
  (unless (node/expr? expr)
    (error (format "desugarExpr called on nonExpr ~a" expr)))

  ; Should always have a currTupIfAtomic when calling
  (mustHaveTupleContext currTupIfAtomic expr)

  (match expr
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent isVar)
     (node/formula/op/in info
                         (list (tup2Expr currTupIfAtomic runContext info) expr))]

    ; atom (base case)
    [(node/expr/atom info arity name)
     (cond
       [currSign
        (node/formula/op/in info
                            (list (tup2Expr currTupIfAtomic runContext info) expr))]
       [else
        (node/formula/op/!
         info (node/formula/op/in
               info
               (list (tup2Expr currTupIfAtomic runContext info) expr)))])]    

    ; The Int constant
    [(node/expr/constant info 1 'Int)
     (node/formula/op/in info
                         (list (tup2Expr currTupIfAtomic runContext info) expr))]

    ; other expression constants
    [(node/expr/constant info arity type)
     (node/formula/op/in info
                         (list (tup2Expr currTupIfAtomic runContext info) expr))]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (desugarExprOp expr quantVars args currTupIfAtomic runContext currSign info)]
 
    ; quantified variable (depends on scope!)
    [(node/expr/quantifier-var info arity sym)     
     (error
      "amalgam: A quantified variable was passed into desugarExpr. Wrong substitution or malformed formula ~n")]

    ; set comprehension e.g. {n : Node | some n.edges}
    ; t in {x0: A0, x1: A1, ... | fmla } means:
    ;   t0 in A0 and t0 in A1 and ... fmla[t0/x0, t1/x1, ...]
    [(node/expr/comprehension info len decls form)
     ; account for multiple variables
     (define vars (map car decls))
     (let ([quantVars (append vars quantVars)])       
       (for-each (lambda (d)
                   ;TODO: Check if this is the right way to pass in target/value 
                   (substituteFormula form quantVars (car d) (cdr d)))
                 decls)
       (desugarFormula form quantVars runContext currSign))]))

(define (desugarExprOp  expr quantVars args
                        currTupIfAtomic runContext currSign info)
  (mustHaveTupleContext currTupIfAtomic expr)
  (match expr

    ; UNION
    [(? node/expr/op/+?)
     ; map over all children of intersection
     ; TODO: we are never creating the in here?
     (define desugaredChildren
       (map
        (lambda (child)
          (desugarExpr child quantVars currTupIfAtomic runContext currSign))
        args))
     ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR
     ;   (currTupIfAtomic in RHS)
     (desugarFormula (node/formula/op/|| info desugaredChildren)
                     quantVars runContext currSign)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (cond
       [(!(equal? (length args) 2))
        (error("Setminus should not be given more than two arguments ~n"))]
       [else
        ; The desugared version of SETMINUS is: (currTupIfAtomic in LHS) and
        ;  (not(currTupIfAtomic in RHS))
        (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext info))
        (define LHS (node/formula/op/in info
                                        (list currTupIfAtomicExpr (first args))))
        (define RHS (node/formula/op/! info
                                       (list node/formula/op/in info
                                             (list currTupIfAtomicExpr
                                                   (second args)))))
        ; Create the final desugared version of SETMINUS by joining LHS and RHS
        ; with an AND and call desugarFormula on it
        (define desugaredSetMinus (node/formula/op/&& info (list LHS RHS)))
        (desugarFormula desugaredSetMinus quantVars runContext currSign)])]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child)
          (desugarExpr child quantVars currTupIfAtomic runContext currSign))
        args))
     ; The desugared version of INTERSECTION is: (currTupIfAtomic in CHILD)
     ; AND (currTupIfAtomic in CHILD)
     (desugarFormula (node/formula/op/&& info desugaredChildren)
                     quantVars runContext currSign)]
    
    ; PRODUCT
    ; TODO: re-implement solution with similar approach as join
    [(? node/expr/op/->?)
     (cond
       [(@>= (node/expr-arity expr) 2)
        (define currentProduct

        ; get product of first thing in args and second thing in args
        (productHelper (list (first args)) (second args)
                          currTupIfAtomic info runContext))

        ; fold over everything else and return the AND of everything in the fold
        (define foldProduct
          (foldl (lambda (curr acc)
                   (productHelper acc curr currTupIfAtomic info runContext))
                 currentProduct (rest (rest args))))
        (node/formula/op/&& info foldProduct)]
       [(equal? (node/expr-arity expr) 1)

        ; if arity is 1, only call desugarExpr on the first thing
        (desugarExpr (first args)
                     quantVars currTupIfAtomic runContext currSign)]
       [else (error
              (format "Expression ~a in product had arity less than 1" expr))])]
    
    ; JOIN
    [(? node/expr/op/join?)
     ; Re-write join as an existentialist formula
     (cond
       [(@>= (node/expr-arity expr) 1)
        ; Example of how to do it like the Java code does:
        ; t in A.B ~~~~> OR_{ta : UB(A), tb: UB(B) | ta.tb = t } (ta in A
        ; and tb in B)
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
                (desugarFormula
                 (node/formula/op/&& info
                                      (list
                                       (node/formula/op/in info
                                                           (list
                                                            taExpr (first args)))
                                       (node/formula/op/in info
                                                           (list
                                                            tbExpr (second args)))))
                 quantVars runContext currSign)]
               [else #f]))
           allPairs))

        (cond
          [(@> (length args) 2)
           (define newJoin (append newArgs
                                   (node/expr/op/join info (node/expr-arity expr)
                                                      (rest (rest args)))))
           (desugarExpr newJoin quantVars currTupIfAtomic runContext currSign)]
          [else newArgs])]
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

     ; TODO: is (first uppers) the correct seconda rgument for extendPossible
     ; Paths
     (define extendResult (extendPossiblePaths uppers (first uppers)))

     ; Check the endpoint and remove items that do not match
     (define endPoint (last (last uppers)))
     
     (define filteredExtendResult
       (filter
        (lambda (result) (equal? (last result) endPoint)) extendResult))
     
     ; Go through everything in filteredExtendResult to create big AND
     ; by calling helper
     (define transitiveAnd
       (transitiveClosureAnd
        filteredExtendResult (first args) info runContext '()))
     (desugarFormula transitiveAnd quantVars runContext currSign)]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (define transitiveClosure
       (node/expr/op/^ info (node/expr-arity (first args)) (first args)))
     (define desugaredRClosure
       (node/expr/op/+ info (node/expr-arity transitiveClosure)
                       (list iden transitiveClosure)))
     (define inFormula
       (node/formula/op/in info
                           (list (tup2Expr currTupIfAtomic runContext info)
                                 desugaredRClosure)))
     (desugarFormula inFormula quantVars runContext currSign)]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (define transposedCurrTupIfAtomic (transposeTup currTupIfAtomic))
     (desugarExpr
      (first args) quantVars transposedCurrTupIfAtomic runContext currSign)]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (error "amalgam: singleton is not supported ~n")]))

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

(define (desugarIntOp expr quantVars args runContext)
  (printf "desugarIntOp~n")
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
     (printf "desugar abs~n")
     (error "amalgam: int abs not supported~n")]
    
    ; signOf 
    [(? node/int/op/sign?)
     (error "amalgam: int signOf not supported~n")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


