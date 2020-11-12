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
(provide desugar-formula)

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
    [(node/formula/multiplicity info mult expr)
     ; create a new multiplicity formula with fields...
     (node/formula/multiplicity info mult (desugar-expr expr quantvars '() runContext currSign))]
    
    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified info quantifier decls form)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (desugar-expr (cdr (car decls)) quantvars '() runContext currSign)     
       (desugar-formula info form quantvars runContext currSign)
       (printf "quant ~a~n" quantifier))]
    
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
       [(and (isGroundProduct leftE) (equal? (length lifted-upper-bounds) 1)) currTupIfAtomic]
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
     (desugar-formula formula quantvars runContext (not currSign))]   

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

; Should always have a currTupIfAtomic when calling
(define (desugar-expr expr quantvars currTupIfAtomic runContext currSign)
  ; Error message to check that we are only taking in expressions
  (unless (node/expr? expr) (error (format "desugar-expr called on non-expr: ~a" expr)))

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
 
    ; quantified variable (depends on scope! which quantifier is this var for?)
    [(node/expr/quantifier-var info arity sym)     
     (printf "  ~a~n" sym)
     (error "amalgam: Something wasn't substituted correctly or the formula was malformed ~n")]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info len decls form)
      ; account for multiple variables  
     (define vars (map car decls))
     (let ([quantvars (append vars quantvars)])       
       (printf "comprehension over ~a~n" vars)
        ; go through each declaration
       (for-each (lambda (d)
                   ;(print-cmd-cont (format "[~a : " (v (get-var-idx (car d) quantvars))))
                   (desugar-expr (cdr d) quantvars currTupIfAtomic runContext currSign)
                   (printf "    decl: ~a~n" d))
                 decls)     
       (desugar-formula form quantvars runContext currSign))]))

(define (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign info)
  (match expr

    ; UNION
    [(? node/expr/op/+?)
     (printf "+~n")
     ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext currTupIfAtomic)
     ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR (currTupIfAtomic in RHS)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; Create the final desugared version of UNION by calling with desguaredChildren
     (define desugaredUnion (node/formula/op/|| info desugaredChildren))
     (desugaredUnion)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
      ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext currTupIfAtomic)
      ; The desugared version of SETMINUS is: (currTupIfAtomic in LHS) iff (not(currTupIfAtomic in RHS))
     (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
     (define LHS (node/formula/op/in info (list currTupIfAtomicExpr (first args))))
     (define RHS (node/formula/op/! info (list node/formula/op/in (list currTupIfAtomicExpr (second args)))))
     ; Recur on the LHS and RHS to see if they need to be desugared further 
     (define desugaredLHS (desugar-expr LHS quantvars currTupIfAtomic runContext currSign))
     (define desugaredRHS (desugar-expr RHS quantvars currTupIfAtomic runContext currSign))
     ; Create the final desugared version of SETMINUS by joining LHS and RHS with an AND 
     (define desugaredSetMinus (node/formula/op/&& info (list desugaredLHS desugaredRHS)))
     desugaredSetMinus]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (printf "& ~a~n" expr)
     ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext currTupIfAtomic)
     ; The desugared version of INTERSECTION is: (currTupIfAtomic in CHILD) AND (currTupIfAtomic in CHILD)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; Create the final desugared version of INTERSECTION by calling with desguaredChildren
     (define desugaredIntersection (node/formula/op/&& info desugaredChildren))
     desugaredIntersection]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     (mustHaveTupleContext currTupIfAtomic)
     (define lef
     ]
    
    ; JOIN
    [(? node/expr/op/join?)
     (printf ".~n")
     (map (lambda (x) (desugar-expr x quantvars '() runContext currSign)) args)
     ]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "^~n")
     (map (lambda (x) (desugar-expr x quantvars '() runContext currSign)) args)
     ]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (printf "*~n")
     ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext currTupIfAtomic)
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
       ;( print-cmd-cont (format "(sum ([~a : ~a " 
       ;                         (v (get-var-idx var quantvars))
       ;                         (if (@> (node/expr-arity var) 1) "set" "one")))
       (desugar-expr (cdr (car decls)) quantvars '() runContext false)
       
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



