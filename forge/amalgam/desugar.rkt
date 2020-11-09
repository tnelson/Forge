;#lang racket
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
;(require "../lang/ast.rkt" (prefix-in @ racket))
;(require "../sigs.rkt")
(require "lift-bounds.rkt")
(provide tup2Expr desugar-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;

; Take in runContext instead of bounds
(define (desugar-formula formula quantvars runContext)
  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant type)
     formula]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op args)
     (desugar-formula-op formula quantvars args runContext)]
    
    ; multiplicity formula (some, one, ...) 
    [(node/formula/multiplicity mult expr)
     ; create a new multiplicity formula with fields...
     (node/formula/multiplicity mult (desugar-expr expr quantvars runContext))]
    
    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified quantifier decls form)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (desugar-expr (cdr (car decls)) quantvars runContext)     
       (desugar-formula form quantvars runContext)
       (printf "quant ~a~n" quantifier))]
    
    ; truth and falsity
    [#t (printf "true~n")]
    [#f (printf "false~n")]
    ))

; This function is recursively calling every element in args and pass it to the
; original recursive function. 
(define (desugar-formula-op formula quantvars args runContext)
  (match formula

    ; AND
    [(? node/formula/op/&&?)
     (printf "and~n")
     ; The desugared version of AND is: to call args recursively
     (define desugaredArgs (map (lambda (x) (desugar-formula x quantvars runContext)) args))
     (node/formula/op/&& (length desugaredArgs) desugaredArgs)
     ]
    
    ; OR
    [(? node/formula/op/||?)
     (printf "or~n")
     ; The desugared version of OR is: to call args recursively
     (define desugaredArgs (map (lambda (x) (desugar-formula x quantvars runContext)) args))
     (node/formula/op/|| (length desugaredArgs) desugaredArgs)
     ]
    
    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "implies~n")
     ; The desugared version of IMPLIES is: (not LHS) OR (RHS)
     (define ante (node/formula/op/!(list (first args))))
     (define conseq (second args))
     (define desugaredImplies (node/formula/op/|| (list ante conseq)))
     (desugar-formula desugaredImplies quantvars runContext)]
    
    ; IN (atomic fmla)
    [(? node/formula/op/in?)
     (printf "in~n")
     ; Doing the non-ground case (e1 in e2) first (TODO: other)
     ;; TODO: need to add the ground case, or the recursion goes forever!
     ; Tim thinks the ground case is: LHS is a single tuple (ground product)
     ; Node0->Node1 in ^edges   <--- this is a ground case of IN! we know the current tuple
     ; Node0->Node1 + Node1->Node2 in ^edges <--- need to turn into an and-of-implications
     ; edges in ~edges <--- same deal, need to build an and-of-implications
     
     ; ^^ FILL IN GROUND CASE HERE (and use a cond to avoid the code below in that case)
     
     ; We don't yet know which relation's bounds will be needed, so just pass them all in
     ;   The bounds-lifter helpers will know what they need and can access the upper bounds then.
     (define leftE (first args)) 
     (define rightE (second args)) ; TODO: descend on these somewhere -- after?      
     (define lifted-upper-bounds (lift-bounds-expr leftE '() runContext)) 
     ;(printf "lub: ~a~n" lifted-upper-bounds)
     ; TODO: other args?
     ; build a big "and" of: for every tuple T in lifted-upper-bounds: (T in leftE) implies (T in rightE)
     (define desugaredAnd (node/formula/op/&& 
                         (map (lambda (x)
                                (define tupExpr (tup2Expr x runContext))
                                (define ante   (node/formula/op/in (list tupExpr leftE)))
                                (define conseq (node/formula/op/in (list tupExpr rightE)))
                                (node/formula/op/=> (list ante conseq))) lifted-upper-bounds)))
     (printf "desugaredAnd: ~a~n" desugaredAnd)
     (desugar-formula desugaredAnd quantvars runContext)]

    ; EQUALS 
    [(? node/formula/op/=?)
     (printf "=~n")
     ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
     (define ante (node/formula/op/in (list (first args) (second args))))
     (define conseq (node/formula/op/in (list (second args) (first args))))
     (define desugaredEquals (node/formula/op/&& (list ante conseq)))
     (desugar-formula desugaredEquals quantvars runContext)]

    ; NEGATION
    [(? node/formula/op/!?)
     (printf "not~n")
      ; The desugared version of NEGATION is: RHS implies false
     (define ante (first args))
     ;; NOTE: TODO: Java version flips a boolean + recurs, negating meaning of operators
     ; need to implement that
     (define conseq false)
     (define desugaredNegation (node/formula/op/=> (list ante conseq)))
     (desugar-formula desugaredNegation quantvars runContext)]   

    ; INTEGER >
    [(? node/formula/op/int>?)
     (printf "int>~n")
     (error "amalgam: int > not supported")
    ]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (printf "int<~n")
     (error "amalgam: int < not supported")
     ]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (printf "int=~n")
     (error "amalgam: int = not supported")
     ]))

(define (desugar-expr expr quantvars runContext)
  (match expr
    ; relation name (base case)
    [(node/expr/relation arity name typelist parent)
     expr]
    ; The Int constant
    [(node/expr/constant 1 'Int)
     expr]
    ; other expression constants
    [(node/expr/constant arity type)
     expr]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op arity args)
     ; currTupIfAtomic is the implicit LHS of the expression
     (desugar-expr-op expr quantvars args (first args) runContext)]

    ; Q: I am a little bit confused about this case 
    ; quantified variable (depends on scope! which quantifier is this var for?)
    [(node/expr/quantifier-var arity sym)     
     ;;(print-cmd-cont (symbol->string (v (get-var-idx expr quantvars))))
     (printf "  ~a~n" sym)]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension len decls form)
      ; account for multiple variables  
     (define vars (map car decls))
     (let ([quantvars (append vars quantvars)])       
       (printf "comprehension over ~a~n" vars)
        ; go through each declaration
       (for-each (lambda (d)
                   ;(print-cmd-cont (format "[~a : " (v (get-var-idx (car d) quantvars))))
                   (desugar-expr (cdr d) quantvars runContext)
                   (printf "    decl: ~a~n" d))
                 decls)     
       (desugar-formula form quantvars runContext))]))

(define (desugar-expr-op expr quantvars args currTupIfAtomic runContext)
  (match expr
    ; Q: Should I be accounting for multiple unions? like in the intersection case below? 
    ; UNION
    [(? node/expr/op/+?)
     (printf "+~n")
     ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR (currTupIfAtomic in RHS)
     (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
     (define ante (node/formula/op/in (list currTupIfAtomicExpr (first args))))
     (define conseq (node/formula/op/in (list currTupIfAtomicExpr (second args))))
     (define desugaredUnion (node/formula/op/|| (list ante conseq)))
     (desugar-formula desugaredUnion quantvars runContext)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
      ; The desugared version of SETMINUS is: (currTupIfAtomic in LHS) AND (not(currTupIfAtomic in RHS))
     (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
     (define ante (node/formula/op/in (list currTupIfAtomicExpr (first args))))
     (define conseq (node/formula/op/! (list node/formula/op/in (list currTupIfAtomicExpr (second args)))))
     (define desugaredSetMinus (node/formula/op/&& (list ante conseq)))
     (desugar-formula desugaredSetMinus quantvars runContext)] 
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (printf "& ~a~n" expr)

     ; The desugared version of INTERSECTION is: (currTupIfAtomic in LHS) AND (currTupIfAtomic in RHS)
     (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
     (define ante (node/formula/op/in (list currTupIfAtomicExpr (first args))))
     (define conseq (node/formula/op/in (list currTupIfAtomicExpr (second args))))
     (define desugaredIntersection (node/formula/op/&& (list ante conseq)))
     (desugar-formula desugaredIntersection quantvars runContext)]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; JOIN
    [(? node/expr/op/join?)
     (printf ".~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "^~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (printf "*~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (printf "~~~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (map (lambda (x) (desugar-int x quantvars runContext)) args)
     ]))

(define (desugar-int expr quantvars runContext)
  (match expr
    ; CONSTANT INT
    [(node/int/constant value)
     (printf "~a~n" value)]
    
    ; apply an operator to some integer expressions
    [(node/int/op args)   
     (desugar-int-op expr quantvars args runContext)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age  
    [(node/int/sum-quant decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ;( print-cmd-cont (format "(sum ([~a : ~a " 
       ;                         (v (get-var-idx var quantvars))
       ;                         (if (@> (node/expr-arity var) 1) "set" "one")))
       (desugar-expr (cdr (car decls)) quantvars runContext)
       
       (desugar-int int-expr quantvars runContext)
       )]))

(define (desugar-int-op expr quantvars args runContext)
  (match expr
    ; int addition
    [(? node/int/op/add?)
     (printf "int+~n")
     (error "amalgam: int + not supported")
     ]
    
    ; int subtraction
    [(? node/int/op/subtract?)
     (printf "int-~n")
     (error "amalgam: int - not supported")
     ]
    
    ; int multiplication
    [(? node/int/op/multiply?)
     (printf "int*~n")
     (error "amalgam: int * not supported")
     ]
    
    ; int division
    [(? node/int/op/divide?)
     (printf "int/~n")
     (error "amalgam: int / not supported")
     ]
    
    ; int sum (also used as typecasting from relation to int)
    ; e.g. {1} --> 1 or {1, 2} --> 3
    [(? node/int/op/sum?)
     (printf "intsum~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; cardinality (e.g., #Node)
    [(? node/int/op/card?)
     (printf "cardinality~n")
     (map (lambda (x) (desugar-expr x quantvars runContext)) args)
     ]
    
    ; remainder/modulo
    [(? node/int/op/remainder?)     
     (error "amalgam: int % (modulo) not supported")
     ]
    
    ; absolute value
    [(? node/int/op/abs?)
     (printf "abs~n")
     (error "amalgam: int abs not supported")
     ]
    
    ; sign-of 
    [(? node/int/op/sign?)
     (printf "sign~n")
     (error "amalgam: int sign-of not supported")
     ]
    ))


(define (tup2Expr tuple context)
  (define tupRelationList
   ; replace every element of the tuple (atoms) with the corresponding atom relation
   (map
    (lambda (tupElem)
      ; keep only the atom relations whose name matches tupElem
      (define filterResult
        (filter (lambda (atomRel)
                  (cond
                    [(and (string? tupElem) (not (symbol? atomRel))) (equal? tupElem (string atomRel))]
                    [(and (string? tupElem) (symbol? atomRel)) (equal? tupElem (symbol->string atomRel))]
                    [(and (not (string? tupElem)) (not (symbol? atomRel))) (equal? (symbol->string tupElem) (number->string atomRel))]
                    [(and (not (string? tupElem)) (symbol? atomRel)) (equal? (symbol->string tupElem) (symbol->string atomRel))]))
                (forge:Run-atoms context)))
      (cond [(equal? 1 (length filterResult)) (first filterResult)]
            [else (error (format "tup2Expr: ~a had <>1 result in atom rels: ~a" tupElem filterResult))]))
    tuple))
  ; TODO: once Tim revises the AST, will need to provide a source location
  (node/expr/op/-> (length tupRelationList) tupRelationList))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



