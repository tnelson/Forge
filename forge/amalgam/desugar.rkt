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
(define (desugar-formula formula quantvars runContext currSign)
  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant info type)
     formula]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     ; We want to pass in the currTupIfAtomic as the implicit LHS for the IN atomic formla case 
     (desugar-formula-op formula quantvars args runContext currSign (first args))]
    
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
(define (desugar-formula-op formula quantvars args runContext currSign currTupIfAtomic)
  (match formula

    ; AND
    [(? node/formula/op/&&?)
     (printf "and~n")
     ; The desugared version of AND is: to call args recursively
     (define desugaredArgs (map (lambda (x) (desugar-formula x quantvars runContext currSign)) args))
     (node/formula/op/&& info (length desugaredArgs) desugaredArgs)
     ]
    
    ; OR
    [(? node/formula/op/||?)
     (printf "or~n")
     ; The desugared version of OR is: to call args recursively
     (define desugaredArgs (map (lambda (x) (desugar-formula x quantvars runContext currSign)) args))
     (node/formula/op/|| info (length desugaredArgs) desugaredArgs)
     ]
    
    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "implies~n")
     ; The desugared version of IMPLIES is: (not LHS) OR (RHS)
     (define ante (node/formula/op/!(list (first args))))
     (define conseq (second args))
     (define desugaredImplies (node/formula/op/|| (list ante conseq)))
     (desugar-formula desugaredImplies quantvars runContext currSign)]

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

     ; we already have the upper bounds Node0 -> Node1 upper bound is just Node0 -> Node1
     ; We don't yet know which relation's bounds will be needed, so just pass them all in
     ;   The bounds-lifter helpers will know what they need and can access the upper bounds then.
     (define leftE (first args))
     (define rightE (second args)) ; TODO: descend on these somewhere -- after?      
     (define lifted-upper-bounds (lift-bounds-expr leftE '() runContext))
     ;(printf "lub: ~a~n" lifted-upper-bounds)
     ; TODO: other args?
     (cond
       ; add (and (is-ground-lhs leftE)
       [(and (is-ground-lhs leftE) (equal? (length lifted-upper-bounds) 1)) currTupIfAtomic]
       [else
        ; build a big "and" of: for every tuple T in lifted-upper-bounds: (T in leftE) implies (T in rightE)
              (define desugaredAnd (node/formula/op/&& 
                         (map (lambda (x)
                                (define tupExpr (tup2Expr x runContext))
                                (define ante   (node/formula/op/in info (list tupExpr leftE)))
                                (define conseq (node/formula/op/in info (list tupExpr rightE)))
                                (node/formula/op/=> (list ante conseq))) lifted-upper-bounds)))
              (printf "desugaredAnd: ~a~n" desugaredAnd)
              (desugar-formula desugaredAnd quantvars runContext currSign)])]

    ; EQUALS 
    [(? node/formula/op/=?)
     (printf "=~n")
     ; The desugared version of EQUALS is: (LHS in RHS) AND (RHS in LHS)
     (define ante (node/formula/op/in (list (first args) (second args))))
     (define conseq (node/formula/op/in (list (second args) (first args))))
     (define desugaredEquals (node/formula/op/&& (list ante conseq)))
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
  (unless (node/expr? expr) (error (format "desugar-expr called on non-expr: ~a" expr)))
  (match expr
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent)
     expr]
    ; The Int constant
    [(node/expr/constant info 1 'Int)
     expr]
    ; other expression constants
    [(node/expr/constant info arity type)
     expr]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign)]
 
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

(define (desugar-expr-op expr quantvars args currTupIfAtomic runContext currSign)
  (match expr
    ; UNION
    [(? node/expr/op/+?)
     (printf "+~n")
     ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext (currTupIfAtomic))
     ; The desugared version of UNION is: (currTupIfAtomic in LHS) OR (currTupIfAtomic in RHS)
     (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
     (define ante (node/formula/op/in (list currTupIfAtomicExpr (first args))))
     (define conseq (node/formula/op/in (list currTupIfAtomicExpr (second args))))
     ; Recur on the LHS and RHS to see if they need to be desugared further 
     (define desugaredAnte (desugar-expr ante quantvars currTupIfAtomic runContext currSign))
     (define desugaredConseq (desugar-expr conseq quantvars currTupIfAtomic runContext currSign))
     ; Create the final desugared version of UNION by joining LHS and RHS with an OR 
     (define desugaredUnion (node/formula/op/|| (list desugaredAnte desugaredConseq)))
     desugaredUnion]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
      ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext (currTupIfAtomic))
      ; The desugared version of SETMINUS is: (currTupIfAtomic in LHS) fff (not(currTupIfAtomic in RHS))
     (define currTupIfAtomicExpr (tup2Expr currTupIfAtomic runContext))
     (define ante (node/formula/op/in (list currTupIfAtomicExpr (first args))))
     (define conseq (node/formula/op/! (list node/formula/op/in (list currTupIfAtomicExpr (second args)))))
     ; Recur on the LHS and RHS to see if they need to be desugared further 
     (define desugaredAnte (desugar-expr ante quantvars currTupIfAtomic runContext currSign))
     (define desugaredConseq (desugar-expr conseq quantvars currTupIfAtomic runContext currSign))
     ; Create the final desugared version of SETMINUS by joining LHS and RHS with an AND 
     (define desugaredSetMinus (node/formula/op/&& (list desugaredAnte desugaredConseq)))
     desugaredSetMinus] 
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (printf "& ~a~n" expr)
     ; Check that the currTupIfAtomic isn't empty 
     (mustHaveTupleContext (currTupIfAtomic))
     ; The desugared version of INTERSECTION is: (currTupIfAtomic in CHILD) AND (currTupIfAtomic in CHILD)
     ; map over all children of intersection
     (define desugaredChildren
       (map
        (lambda (child) (desugar-expr child quantvars currTupIfAtomic runContext currSign)) args))
     ; Create the final desugared version of INTERSECTION by calling with desguaredChildren
     (define desugaredIntersection (node/formula/op/&& desugaredChildren))
     desugaredIntersection]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     (map (lambda (x) (desugar-expr x quantvars '() runContext currSign)) args)
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
     (mustHaveTupleContext (currTupIfAtomic))
     ; The desugared version of REFLEXIVE-TRANSITIVE CLOSURE is ((iden) & (univ->univ)) + (^expr) 
     (define productOfUniv (node/expr/op/-> (list univ univ)))
     (define restrictedIden (node/expr/op/& (list iden productOfUniv)))
     ; Q: What is our specific input? 
     (define transitiveClosure (node/expr/op/^ (args)))
     ; Q: Do we want to call this recursively ?
     (define desugaredRClosure (node/expr/op/+ (list restrictedIden transitiveClosure)))
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

; Helper to transform a given tuple from the lifted-upper bounds function to a relation, and then do the product of all relations
; to form an expression. 
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

; Helper used to flip the currentTupleIfAtomic in the transpose case 
(define (transposeTup tuple)
  (cond 
         [(equal? (length(tuple)) 2) ((list (second tuple) (first tuple)))]
         [else (error "transpose tuple for tup ~a isn't arity 2. It has arity ~a" tuple (length(tuple)))]))

; This helper checks the value of currTupIfAtomic and throws an error if it is empty. 
(define (mustHaveTupleContext tup)
  (cond
    [(equal? (length tup) 0) (error "currTupIfAtomic is empty and it shouldn't be")]))

; Used to determine if we are at the base case
(define (is-ground-lhs leftE)
  (cond
    [(unary-op? leftE) ...]
    [(binary-op? leftE) (if (node/expr/op/->? leftE) (and (is-ground-lhs (first leftE))  (is-ground-lhs (second leftE))))]
    ; TODO: ExprVar case
    [(node/expr/constant? leftE) (if (number? (node/expr/constant leftE)) true)]
    [else (false)])
  (match leftE
    [unary-op]
    )) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



