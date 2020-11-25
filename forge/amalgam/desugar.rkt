#lang forge/core

; Desugaring functions for Amalgam
; (full AST) -> (restricted AST without stuff like implies)
;    Note: These functions maintain an environment of
;    quantified variables to aid general functionality
; We are bringing the input into our core language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Warning: ast.rkt exports (e.g.) "and".
; This is the macro that produces an "and" formula!
; To use real Racket and, use @and.

(require "lift-bounds.rkt")
(require "desugar_helpers.rkt")
(require "substitutor.rkt")
(provide desugar-formula)
(require debug/repl)

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugar-formula formula quantvars runContext currSign)
  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant info type)
     (printf "desugar constant formula ~a~n" formula)
     formula]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     ; We want to pass in the currTupIfAtomic as the implicit LHS
     (desugar-formula-op formula quantvars args runContext currSign (first args) info)]
    
    ; multiplicity formula (some, one, ...)
    ; desugar some e to quantified fmla some x_fresh : union(upperbound(e)) | x_fresh in e
    [(node/formula/multiplicity info mult expr)
     (printf "desugar mult ~a~n" mult)
     (define freshvar (node/expr/quantifier-var info 1 (list (gensym "m2q"))))
     (define uppers (lift-bounds-expr expr quantvars runContext))
     ;uppers is a list of tuples 
     (define unionOfBounds (node/expr/op/+ info (length uppers) (map (lambda (tup)
                                                                       (tup2Expr tup runContext)) uppers)))
     (printf "desugar union of Bounds ~a ~n" unionOfBounds)
     (define domain unionOfBounds) 
     (define newfmla (node/formula/op/in info (list freshvar expr)))
     (define newdecls (list (cons freshvar domain)))
     (define desugaredMultiplicity (node/formula/quantified info mult newdecls newfmla))
     (printf "desugar new mult ~a~n" desugaredMultiplicity)
     (desugar-formula desugaredMultiplicity quantvars runContext currSign)]
    

    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified info quantifier decls form)
     (printf "desugar quant ~a~n" quantifier)
     (printf "desugar quant formula ~a~n" formula)

     ; In the case where the quantifier is not a 'some or 'all, desugar into somes/alls  
     (cond [(not (or (equal? quantifier 'some)
                     (equal? quantifier 'all)))
            (cond
              ; no x: A | r.x in q ------> all x: A | not (r.x in q)
              [(equal? quantifier 'no)
               (define negatedFormula (node/formula/op/! info (list form)))
               (define newQuantFormula (node/formula/quantified info 'all decls negatedFormula))
               (desugar-formula newQuantFormula quantvars runContext currSign)]

              ; one x: A | r.x in q ------> (some x: A | r.x in q and (all y: A-x | not (r.x in q)))
              [(equal? quantifier 'one)
               (define newQuantFormLHS (node/formula/quantified info 'some decls form))
               (define negatedFormula (node/formula/op/! info (list form)))
               (define subtractedDecls (node/expr/op/- info 2 (list (cdr decls) (car decls))))
               (define newQuantFormRHS (node/formula/quantified info 'all subtractedDecls negatedFormula))
               (define desugaredAnd (node/formula/op/&& info (list newQuantFormLHS newQuantFormRHS)))
               (desugar-formula desugaredAnd quantvars runContext currSign)]

              ; lone x: A | r.x in q ------> (no x: A | r.x in q) or (one x: A | r.x in q)
              [(equal? quantifier 'lone)
               (define newQuantFormLHS (node/formula/quantified info 'no decls form))
               (define newQuantFormRHS (node/formula/quantified info 'one decls form))
               (define desugaredOR (node/formula/op/|| info (list newQuantFormLHS newQuantFormRHS)))
               (desugar-formula desugaredOR quantvars runContext currSign)])]

           ; if it's got multiple variables, foldl over the helper that gets big AND or OR of subformulas 
           [(not (equal? 1 (length decls)))
            
            (when (and (not (equal? (quantifier 'and))) (not (equal? (quantifier 'some))))
                        (error (format "Multiple quantifiers with something other than all/some: ~a" form)))

            (define currQuantifier (list (createNewQuantifier (first decls) quantvars form runContext info quantifier formula)))
            (define quants (foldl (lambda (curr acc) (append (createNewQuantifier curr quantvars form runContext info quantifier formula) acc))
                   currQuantifier (rest decls)))
            (define unionOfQuants (node/formula/op/|| info quants))
            (desugar-formula unionOfQuants quantvars runContext currSign)]

           [else
            (define newFormula (createNewQuantifier decls quantvars form runContext info quantifier formula))
            (desugar-formula newFormula quantvars runContext currSign)])]

    ; truth and falsity
    [#t (printf "desugar true~n")]
    [#f (printf "desugar false~n")]
    ))

; This function is recursively calling every element in args and pass it to the
; original recursive function. 
(define (desugar-formula-op formula quantvars args runContext currSign currTupIfAtomic info)
  (match formula

    ; AND 
    [(? node/formula/op/&&?) 
     (printf "desugar and~n")
     (define desugaredArgs
       (map (lambda (x) (desugar-formula x quantvars runContext currSign)) args))
     (cond
       [(currSign) (node/formula/op/&& info desugaredArgs)]
       [else (node/formula/op/|| info desugaredArgs)])]

    ; OR
    [(? node/formula/op/||?)
     (printf "desugar or~n")
     (define desugaredArgs
       (map (lambda (x) (desugar-formula x quantvars runContext currSign)) args))
     (cond
       [(currSign) (node/formula/op/|| info desugaredArgs)]
       [else (node/formula/op/&& info desugaredArgs)])]

    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "desugar implies~n")
     ; The desugared version of IMPLIES is: (not LHS) OR (RHS)
     (define ante (node/formula/op/! info (list (first args))))
     (define conseq (second args))
     (define desugaredImplies (node/formula/op/|| info (list ante conseq)))
     (desugar-formula desugaredImplies quantvars runContext currSign)]

    ; IN (atomic fmla)
    ; This function has two cases, the ground case and the case where we build an and-of-implications.
    ; Some examples can be seen below: 
    ;     Node0->Node1 in ^edges   <--- this is a ground case of IN! we know the current tuple
    ;     Node0->Node1 + Node1->Node2 in ^edges <--- need to turn into an and-of-implications
    ;     edges in ~edges <--- same deal, need to build an and-of-implications
    [(? node/formula/op/in?)
     (printf "desugar in~n")

     (define leftE (first args))
     (define rightE (second args))
     
     ; We don't yet know which relation's bounds will be needed, so just pass them all in
     (define lifted-upper-bounds (lift-bounds-expr leftE '() runContext))
     
     (cond
       [(and (isGroundProduct leftE) (equal? (length lifted-upper-bounds) 1))
        (desugar-expr leftE quantvars currTupIfAtomic runContext currSign)]
       [else
        ; build a big "and" of: for every tuple T in lifted-upper-bounds: (T in leftE) implies (T in rightE)
        (define desugaredAnd (node/formula/op/&& info
                                                 (map (lambda (x)
                                                        (define tupExpr (tup2Expr x runContext))
                                                        (define LHS   (node/formula/op/in info (list tupExpr leftE)))
                                                        (define RHS (node/formula/op/in info (list tupExpr rightE)))
                                                        (node/formula/op/=> info (list LHS RHS))) lifted-upper-bounds))) 
        (printf "desugaredAnd: ~a~n" desugaredAnd)
        (desugar-formula desugaredAnd quantvars runContext currSign)])]

    ; EQUALS 
    [(? node/formula/op/=?)
     (printf "desugar =~n")
     ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
     (define LHS (node/formula/op/in info (list (first args) (second args))))
     (define RHS (node/formula/op/in info (list (second args) (first args))))
     (define desugaredEquals (node/formula/op/&& info (list LHS RHS)))
     (desugar-formula desugaredEquals quantvars runContext currSign)]

    ; NEGATION
    [(? node/formula/op/!?)
     (printf "desugar not~n")
     ; The desugared version of NEGATION is to flip the currSign type
     (desugar-formula (first args) quantvars runContext (not currSign))]   

    ; INTEGER >
    [(? node/formula/op/int>?)
     (error "amalgam: int > not supported ~n")]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (error "amalgam: int < not supported ~n")]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (error "amalgam: int = not supported ~n")]))

(define (desugar-expr expr quantvars currTupIfAtomic runContext currSign)

  ; Error message to check that we are only taking in expressions
  (unless (node/expr? expr) (error (format "desugar-expr called on non-expr: ~a" expr)))

  ; Should always have a currTupIfAtomic when calling
  (mustHaveTupleContext currTupIfAtomic)

  (match expr
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent)
     (printf "desugar relation name ~n")
     (node/formula/op/in info (list currTupIfAtomic expr))]

    ; atom (base case)
    [(node/expr/relation info arity name typelist parent)
     (printf "desugar atom base case ~n")
     (cond
       [currSign (node/formula/op/in info (list currTupIfAtomic expr))]
       [else (node/formula/op/! info (node/formula/op/in info (list currTupIfAtomic expr)))])]    

    ; The Int constant
    [(node/expr/constant info 1 'Int)
     (printf "desugar int constant ~n")
     (node/formula/op/in info (list currTupIfAtomic expr))]

    ; other expression constants
    [(node/expr/constant info arity type)
     (printf "desugar expression constants ~n")
     (node/formula/op/in info (list currTupIfAtomic expr))]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (printf "desugar expression with operators ~n")
     (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign info)]
 
    ; quantified variable (depends on scope!)
    [(node/expr/quantifier-var info arity sym)     
     (printf "quantified variable found in desugar ~a~n" sym)
     (error "amalgam: A quantified variable was passed into desugar-expr. Wrong substitution or malformed formula ~n")]

    ; set comprehension e.g. {n : Node | some n.edges}
    ; t in {x0: A0, x1: A1, ... | fmla } means:
    ;   t0 in A0 and t0 in A1 and ... fmla[t0/x0, t1/x1, ...]
    [(node/expr/comprehension info len decls form)
     (printf "desugar set comprehension ~n")
     ; account for multiple variables
     (define vars (map car decls))
     (let ([quantvars (append vars quantvars)])       
       (for-each (lambda (d)
                   ;TODO: Check if this is the right way to pass in target/value 
                   (substitute-formula form quantvars (car d) (cdr d)))
                 decls)
       (desugar-formula form quantvars runContext currSign))]))

(define (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign info)
  (mustHaveTupleContext currTupIfAtomic)
  (match expr

    ; UNION
    [(? node/expr/op/+?)
     (printf "desugar +~n")
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR (currTupIfAtomic in RHS)
     (node/formula/op/|| info desugaredChildren)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (printf "desugar -~n")
     (cond
       [(!(equal? (length args) 2)) (error("Setminus should not be given more than two arguments ~n"))]
       [else 
        ; The desugared version of SETMINUS is: (currTupIfAtomic in LHS) and (not(currTupIfAtomic in RHS))
        (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
        (define LHS (node/formula/op/in info (list currTupIfAtomicExpr (first args))))
        (define RHS (node/formula/op/! info (list node/formula/op/in info (list currTupIfAtomicExpr (second args)))))
        ; Create the final desugared version of SETMINUS by joining LHS and RHS with an AND and call desugar-formula on it
        (define desugaredSetMinus (node/formula/op/&& info (list LHS RHS)))
        (desugar-formula desugaredSetMinus quantvars runContext currSign)])]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (printf "desugar & ~a~n" expr)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; The desugared version of INTERSECTION is: (currTupIfAtomic in CHILD) AND (currTupIfAtomic in CHILD)
     (node/formula/op/&& info desugaredChildren)]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "desugar ->~n")
     (cond
       [(equal? (node/expr-arity expr) 2)
        (define LHS (first args))
        (define RHS (second args))
        (define leftTupleContext  (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
        (define rightTupleContext (projectTupleRange currTupIfAtomic (node/expr-arity LHS) (node/expr-arity RHS)))
        (define formulas (list
                          (node/formula/op/in info (list (tup2Expr leftTupleContext) LHS))
                          (node/formula/op/in info (list (tup2Expr rightTupleContext) RHS))))
        (node/formula/op/&& info formulas)]
       [else (error (format "Expression ~a in product had arity greater than 2") expr)])]
    
    ; JOIN
    [(? node/expr/op/join?)
     (printf "desugar .~n")
     ; Re-write join as an existentialist formula
     (cond
       [(equal? (node/expr-arity expr) 2)
        (define rightColLHS (getColumnRight (first args)))
        (define leftColRHS (getColumnLeft (second args)))
        (define listOfColumns (list rightColLHS leftColRHS))
        (define intersectColumns (node/expr/op/& info (length listOfColumns) listOfColumns))
        (define joinNode (node/formula/multiplicity info 'some intersectColumns))
        (define LHSRange (projectTupleRange joinNode 0 (node/expr-arity (- (first args) 1))))
        (define RHSRange (projectTupleRange joinNode (node/expr-arity (first args)) (node/expr-arity (second args))))
        (define LHSProduct (node/expr/op/-> info (length (list LHSRange joinNode)) (list LHSRange joinNode)))
        (define RHSProduct (node/expr/op/-> info (length (list joinNode RHSRange)) (list joinNode RHSRange)))
        (define LHSIn (node/formula/op/in info (list LHSProduct (first args))))
        (define RHSIn (node/formula/op/in info (list RHSProduct (second args))))
        (define finalAnd (node/formula/op/&& info (list LHSIn RHSIn)))
        (desugar-formula finalAnd quantvars runContext currSign)]
       [else (error (format "Expression ~a in join had arity greater than 2") expr)])]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "desugar ^~n")
     ; TODO: Complete the transitive closure case 
     (map (lambda (x) (desugar-expr x quantvars '() runContext currSign)) args)]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (printf "desugar *~n")
     ; The desugared version of REFLEXIVE-TRANSITIVE CLOSURE is ((iden) & (univ->univ)) + (^expr) 
     (define productOfUniv (node/expr/op/-> info 2 (list 'univ 'univ)))
     (define restrictedIden (node/expr/op/& info 2 (list 'iden productOfUniv)))
     (define transitiveClosure (node/expr/op/^ info 1 (first args)))
     ; TODO: Do we want to call this recursively ?
     (define desugaredRClosure (node/expr/op/+ info 2 (list restrictedIden transitiveClosure)))
     (desugaredRClosure)]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (printf "desugar ~~~n")
     (define transposedCurrTupIfAtomic (transposeTup currTupIfAtomic))
     (desugar-expr expr quantvars (first args) transposedCurrTupIfAtomic runContext currSign)]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     ; TODO: Complete this case 
     (printf "desugar sing~n")
     (map (lambda (x) (desugar-int x quantvars runContext)) args)]))

(define (desugar-int expr quantvars runContext)
  (match expr
    ; CONSTANT INT
    [(node/int/constant info value)
     ; TODO: we should be returning something here instead of just printing something 
     (printf "desugar constant int~a~n" value)]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)
     (printf "desugar int operator ~n")
     (desugar-int-op expr quantvars args runContext)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age
    ; TODO: Make sure this case is OK --> with substitution specifically 
    [(node/int/sum-quant info decls int-expr)
     (printf "desugar sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (for-each (lambda (d)
                   ; the target is the variable x, and the value is Node. I think this might be wrong.
                   ; if we have x: Node, how do we get all of the instances of Node? 
                   (substitute-expr expr quantvars (car d) (cdr d)))
                 decls)           
       (desugar-int int-expr quantvars runContext))]))

(define (desugar-int-op expr quantvars args runContext)
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
     (printf "desugar cardinality~n")
     (map (lambda (x) (desugar-expr x quantvars (first args) runContext false)) args)]
    
    ; remainder/modulo
    [(? node/int/op/remainder?)     
     (error "amalgam: int % (modulo) not supported~n")]
    
    ; absolute value
    [(? node/int/op/abs?)
     (error "amalgam: int abs not supported~n")]
    
    ; sign-of 
    [(? node/int/op/sign?)
     (error "amalgam: int sign-of not supported~n")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


