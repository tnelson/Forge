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
     formula]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     ; We want to pass in the currTupIfAtomic as the implicit LHS
     (desugar-formula-op formula quantvars args runContext currSign (first args) info)]
    
    ; multiplicity formula (some, one, ...)
    ; desugar some e to quantified fmla some x_fresh : union(upperbound(e)) | x_fresh in e
    [(node/formula/multiplicity info mult expr)
     (define freshvar (node/expr/quantifier-var 1 info (gensym "m2q")))
     (define domain univ)  ; TODO: union over upper-bound of e
     (define newfmla (node/formula/op/in info (list freshvar expr)))
     (define newdecls (list (cons freshvar domain)))
     (define desugaredMultiplicity (node/formula/quantified info mult newdecls newfmla))
     (desugar-formula desugaredMultiplicity quantvars runContext currSign)]
    

    
    ; quantified formula (some x : ... or all x : ...)
    ; if it's a complex quantifier (no, lone, one) that adds constraints, first desugar into somes/alls
    ; if it's got multiple variables, first split into multiple single-var quantifiers
    [(node/formula/quantified info quantifier decls form)
     (printf "quant ~a~n" quantifier)
     (debug-repl)

     ; QUANT DECLS | SUBFMLA
     ;       DECLS = ((x . A))
     ; no x: A | r.x in q ------> all x: A | not (r.x in q)
     ; one x: A | r.x in q ------> (some x: A | r.x in q and (all y: A-x | not (r.x in q)))
     ; lone x: A | r.x in q ------> (no x: A | r.x in q) or (one x: A | r.x in q)
     (cond [(not (or (equal? quantifier 'some)
                     (equal? quantifier 'all)))
            ; TODO: desugar this quantifier type
            ; e.g. lone x:A | ...
            ; see above
            (cond
              [(equal? quantifier 'no)]
              [(equal? quantifier 'one)]
              [(equal? quantifier 'lone)])
            ]
           [(not (equal? 1 (length decls)))
            ; TODO: desugar this multi-var quantifier
            ; some x: A, y: B | x.y in q
            ;   ^ (1) turn below into a helper and fold that helper over the decls
            ;     (2) just break up the decls. e.g., some x: A | some y: B | x.y in q

            ; The problem:
            ; (F1) one x: A, y: B | x->y in roads
            ;   IS NOT EQUIVALENT TO
            ; (F2) one x: A | one y: B | x->y in roads
            ;  F1 means "there is exactly one tuple <x, y> such that x->y in roads
            ;     i.e., one tuple in roads relation
            ;  F2 means "there is exactly one x, such that there is exactly one y, such that x->y in roads
            ;     i.e., only one city with one outgoing road
            
            ]
           [else 
            (define var (car (car decls)))
            (define domain (cdr (car decls)))
            (let ([quantvars (cons var quantvars)])
              
              ; the target is the variable x, and the value is Node. I think this might be wrong.
              ; if we have x: Node, how do we get all of the instances of Node?
       
              ; gives us list of all possible bindings to this variable
              (define lifted-bounds (lift-bounds-expr domain quantvars runContext))
              ; produce a list of subformulas each substituted with a possible binding for the variable
              (define subformulas
                (map
                 (lambda (tup) (substitute-formula form quantvars var (tup2Expr tup runContext info)))
                 lifted-bounds))
              (cond [(equal? quantifier 'some) (node/formula/op/|| info subformulas)]
                    [(equal? quantifier 'all) (node/formula/op/&& info subformulas)]
                    [else (error (format "desugaring unsupported: ~a" formula))]))
            ;(desugar-expr (cdr (car decls)) quantvars '() runContext currSign)
            (desugar-formula form quantvars runContext currSign)])]

    ; truth and falsity
    [#t (printf "true~n")]
    [#f (printf "false~n")]
    ))

; This function is recursively calling every element in args and pass it to the
; original recursive function. 
(define (desugar-formula-op formula quantvars args runContext currSign currTupIfAtomic info)
  (match formula

    ; AND 
     [(? node/formula/op/&&?) 
     (printf "and~n")
     (define desugaredArgs (map (lambda (x) (desugar-formula x quantvars runContext currSign)) args))
     (cond
       [(currSign) (node/formula/op/&& info (length desugaredArgs) desugaredArgs)]
       [else (node/formula/op/|| info (length desugaredArgs) desugaredArgs)])
     ]

    ; OR
     [(? node/formula/op/||?)
     (printf "or~n")
     (define desugaredArgs (map (lambda (x) (desugar-formula x quantvars runContext currSign)) args))
     (cond
       [(currSign) (node/formula/op/|| info (length desugaredArgs) desugaredArgs)]
       [else (node/formula/op/&& info (length desugaredArgs) desugaredArgs)])
     ]

    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "implies~n")
     ; The desugared version of IMPLIES is: (not LHS) OR (RHS)
     (define ante (node/formula/op/! info (list (first args))))
     (define conseq (second args))
     (define desugaredImplies (node/formula/op/|| info (list ante conseq)))
     (desugar-formula desugaredImplies quantvars runContext currSign)]

    ; IN (atomic fmla)
    [(? node/formula/op/in?)
     (printf "in~n")

     ; In this function there are two cases, the ground case and the case where we build an and-of-implications.
     ; Some examples can be seen below: 
     ;     Node0->Node1 in ^edges   <--- this is a ground case of IN! we know the current tuple
     ;     Node0->Node1 + Node1->Node2 in ^edges <--- need to turn into an and-of-implications
     ;     edges in ~edges <--- same deal, need to build an and-of-implications
  
     (define leftE (first args))
     (define rightE (second args))
     
     ; we already have the upper bounds Node0 -> Node1 upper bound is just Node0 -> Node1
     ; We don't yet know which relation's bounds will be needed, so just pass them all in
     ;   The bounds-lifter helpers will know what they need and can access the upper bounds then.
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
     (printf "=~n")
     ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
     (define LHS (node/formula/op/in info (list (first args) (second args))))
     (define RHS (node/formula/op/in info (list (second args) (first args))))
     (define desugaredEquals (node/formula/op/&& info (list LHS RHS)))
     (desugar-formula desugaredEquals quantvars runContext currSign)]

    ; NEGATION
    [(? node/formula/op/!?)
     (printf "not~n")
     ; The desugared version of NEGATION is to flip the currSign type
     (desugar-formula (first args) quantvars runContext (not currSign))]   

    ; INTEGER >
    [(? node/formula/op/int>?)
     (printf "int>~n")
     (error "amalgam: int > not supported ~n")
    ]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (printf "int<~n")
     (error "amalgam: int < not supported ~n")
     ]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (printf "int=~n")
     (error "amalgam: int = not supported ~n")
     ]))

(define (desugar-expr expr quantvars currTupIfAtomic runContext currSign)
  ; Error message to check that we are only taking in expressions
  (unless (node/expr? expr) (error (format "desugar-expr called on non-expr: ~a" expr)))
  ; Should always have a currTupIfAtomic when calling
  (mustHaveTupleContext currTupIfAtomic)

  (match expr
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent)
      (node/formula/op/in info (list currTupIfAtomic expr))]

    ; The Int constant
    [(node/expr/constant info 1 'Int)
     (node/formula/op/in info (list currTupIfAtomic expr))]

    ; other expression constants
    [(node/expr/constant info arity type)
     (node/formula/op/in info (list currTupIfAtomic expr))]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign info)]
 
    ; quantified variable (depends on scope!)
    [(node/expr/quantifier-var info arity sym)     
     (printf "  ~a~n" sym)
     (error "amalgam: Something wasn't substituted correctly or the formula was malformed ~n")]

    ; set comprehension e.g. {n : Node | some n.edges}
    ; t in {x0: A0, x1: A1, ... | fmla } means:
    ;   t0 in A0 and t0 in A1 and ... fmla[t0/x0, t1/x1, ...]
    [(node/expr/comprehension info len decls form)
      ; account for multiple variables
     (define vars (map car decls))
     (let ([quantvars (append vars quantvars)])       
       (for-each (lambda (d)
                   ; the target is the variable x, and the value is Node. I think this might be wrong.
                   ; if we have x: Node, how do we get all of the instances of Node? 
                   (substitute-formula form quantvars (car d) (cdr d)))
                 decls)     
       (desugar-formula form quantvars runContext currSign))]))

(define (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign info)
  (mustHaveTupleContext currTupIfAtomic)
  (match expr

    ; UNION
    [(? node/expr/op/+?)
     (printf "+~n")
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; Create the final desugared version of UNION by calling with desguaredChildren
     ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR (currTupIfAtomic in RHS)
     (define desugaredUnion (node/formula/op/|| info desugaredChildren))
     (desugaredUnion)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
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
     (printf "& ~a~n" expr)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; Create the final desugared version of INTERSECTION by calling with desguaredChildren
     ; The desugared version of INTERSECTION is: (currTupIfAtomic in CHILD) AND (currTupIfAtomic in CHILD)
     (define desugaredIntersection (node/formula/op/&& info desugaredChildren))
     desugaredIntersection]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     (cond
       [(equal? (node/expr-arity expr) 2)
         (define LHS (first args))
         (define RHS (second args))
         (define leftTupleContext  (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
         (define rightTupleContext (projectTupleRange currTupIfAtomic (node/expr-arity LHS) (node/expr-arity RHS)))
         (define formulas (list
                           (node/formula/op/in info (list (tup2Expr leftTupleContext) LHS))
                           (node/formula/op/in info (list (tup2Expr rightTupleContext) RHS))))
         (define desugaredProduct (node/formula/op/&& info formulas))
         desugaredProduct]
       [else (error (format "Expression ~a in product had arity greater than 2") expr)])]
    
    ; JOIN
    [(? node/expr/op/join?)
     (printf ".~n")
     ; re-write join as an existentialist formula
     (cond
       [(equal? (node/expr-arity expr) 2)
        (define rightColLHS (getColumnRight (first args)))
        (define leftColRHS (getColumnLeft (second args)))
        (define intersectColumns (node/expr/op/& info (list rightColLHS leftColRHS)))
        (define joinNode (node/formula/multiplicity info 'some intersectColumns))
        (define LHSRange (projectTupleRange joinNode 0 (node/expr-arity (- (first args) 1))))
        (define RHSRange (projectTupleRange joinNode (node/expr-arity (first args)) (node/expr-arity (second args))))
        (define LHSProduct (node/expr/op/-> info (list LHSRange joinNode)))
        (define RHSProduct (node/expr/op/-> info (list joinNode RHSRange)))
        (define LHSIn (node/formula/op/in info (list LHSProduct (first args))))
        (define RHSIn (node/formula/op/in info (list RHSProduct (second args))))
        (define finalAnd (node/formula/op/&& info (list LHSIn RHSIn)))
        (desugar-formula finalAnd quantvars runContext currSign)]
       [else (error (format "Expression ~a in join had arity greater than 2") expr)])]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "^~n")
     (map (lambda (x) (desugar-expr x quantvars '() runContext currSign)) args)
     ]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (printf "*~n")
     ; The desugared version of REFLEXIVE-TRANSITIVE CLOSURE is ((iden) & (univ->univ)) + (^expr) 
     (define productOfUniv (node/expr/op/-> info (list univ univ)))
     (define restrictedIden (node/expr/op/& info (list iden productOfUniv)))
     (define transitiveClosure (node/expr/op/^ info (first args)))
     ; Q: Do we want to call this recursively ?
     (define desugaredRClosure (node/expr/op/+ info (list restrictedIden transitiveClosure)))
     (desugaredRClosure)
     ]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (printf "~~~n")
     (define transposedCurrTupIfAtomic (transposeTup(currTupIfAtomic)))
     ; for ~edges, args contains edges
     (desugar-expr expr quantvars (first args) transposedCurrTupIfAtomic runContext currSign)
     ]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (map (lambda (x) (desugar-int x quantvars runContext)) args)
     ]))

(define (desugar-int expr quantvars runContext)
  (match expr
    ; CONSTANT INT
    [(node/int/constant info value)
     (printf "~a~n" value)]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)   
     (desugar-int-op expr quantvars args runContext)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age  
    [(node/int/sum-quant info decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ;This is what we had before: (desugar-expr (cdr (car decls)) quantvars '() runContext false)
       (for-each (lambda (d)
                   ; the target is the variable x, and the value is Node. I think this might be wrong.
                   ; if we have x: Node, how do we get all of the instances of Node? 
                   (substitute-expr expr quantvars (car d) (cdr d)))
                 decls)           
       (desugar-int int-expr quantvars runContext)
       )]))

(define (desugar-int-op expr quantvars args runContext)
  (match expr
    ; int addition
    [(? node/int/op/add?)
     (printf "int+~n")
     (error "amalgam: int + not supported~n")
     ]
    
    ; int subtraction
    [(? node/int/op/subtract?)
     (printf "int-~n")
     (error "amalgam: int - not supported~n")
     ]
    
    ; int multiplication
    [(? node/int/op/multiply?)
     (printf "int*~n")
     (error "amalgam: int * not supported~n")
     ]
    
    ; int division
    [(? node/int/op/divide?)
     (printf "int/~n")
     (error "amalgam: int / not supported ~n")
     ]
    
    ; int sum (also used as typecasting from relation to int)
    ; e.g. {1} --> 1 or {1, 2} --> 3
    [(? node/int/op/sum?)
     (printf "intsum~n")
      (error "amalgam: sum not supported ~n")
     ]
    
    ; cardinality (e.g., #Node)
    [(? node/int/op/card?)
     (printf "cardinality~n")
     (map (lambda (x) (desugar-expr x quantvars (first args) runContext false)) args)
     ]
    
    ; remainder/modulo
    [(? node/int/op/remainder?)     
     (error "amalgam: int % (modulo) not supported~n")
     ]
    
    ; absolute value
    [(? node/int/op/abs?)
     (printf "abs~n")
     (error "amalgam: int abs not supported~n")
     ]
    
    ; sign-of 
    [(? node/int/op/sign?)
     (printf "sign~n")
     (error "amalgam: int sign-of not supported~n")
     ]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


