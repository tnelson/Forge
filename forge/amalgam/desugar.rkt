#lang racket

; Desugaring functions for Amalgam
; (full AST) -> (restricted AST without stuff like implies)
;    Note: These functions maintain an environment of
;    quantified variables to aid general functionality
; We are bringing the input into our core language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Warning: ast.rkt exports (e.g.) "and".
; This is the macro that produces an "and" formula!
; To use real Racket and, use @and.
(require "../lang/ast.rkt" (prefix-in @ racket))
(require "../sigs.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugar-formula formula quantvars)
  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant type)
     formula]
    ; operator formula (and, or, implies, ...)
    [(node/formula/op args)
     (desugar-formula-op formula quantvars args)]
    ; multiplicity formula (some, one, ...) 
    [(node/formula/multiplicity mult expr)
     ; create a new multiplicity formula with fields...
     (node/formula/multiplicity mult (desugar-expr expr quantvars))]
    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified quantifier decls form)
     ; car returns the first element of the pair
     (define var (car (car decls)))
     ; cons is appending var into quantvars list 
     (let ([quantvars (cons var quantvars)])
       ; cdr returns everything but the first entry in the list  
       (desugar-expr (cdr (car decls)) quantvars)     
       (desugar-formula form quantvars)
       (printf "quant ~a~n" quantifier))]
    ; truth and falsity
    ; TODO: wat
    [#t (printf "true~n")]
    [#f (printf "false~n")]
    ))

; This function is recursively calling every element in args and pass it to the
; original recursive function. 
(define (desugar-formula-op formula quantvars args)
  (match formula
    ; ? <test> matches on forms that <test> returns true for

    ; AND
    [(? node/formula/op/&&?)
     (printf "and~n")
     ; applying desugar-formula x quantvars to everything in args
     (map (lambda (x) (desugar-formula x quantvars)) args)
     ]
    ; OR
    [(? node/formula/op/||?)
     (printf "or~n")
     ; applying desugar-formula x quantvars to everything in args
     (map (lambda (x) (desugar-formula x quantvars)) args)
     ]    
    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "implies~n")
     ; Implies should be desugared as (not LHS) OR (RHS) 
     (let ([desugaredImplies (node/formula/op/|| (list (node/formula/op/!(list (first args))) (second args)))])
       ; Call desugar-formula recursively on the desugared expression created on the previous step 
      (desugar-formula desugaredImplies quantvars))
     ]
    ; IN (atomic fmla)
    ; Q: Can we please go over this case together? 
    [(? node/formula/op/in?)
     (printf "in~n")
     ; Doing the non-ground case (e1 in e2) first (TODO: other)
     
     ; We don't yet know which relation's bounds will be needed, so just pass them all in
     ;   The bounds-lifter helpers will know what they need and can access the upper bounds then.
     (define leftE (first args)) 
     (define rightE (second args)) ; TODO: descend on these somewhere -- after?
     ; lift-bounds-expr should take *full kodkod bounds* and produce just an upper-bound
     ;(define lifted-upper-bounds (lift-bounds-expr leftE kk-bounds '())) ; List<List<Atom>> i.e., List<Tuple>     

     ; for tuple2Expr: (forge:Run-atom-rels foo5) run foo5 for 5 
     ;  ^ build the product of the result of converting each atom to its corresponding relation from this list
     ; rel(1st tuple) x rel(2nd  tuple) x .... x (rel nth tuple) --> expression
     
     ; TODO: other args?
     ; build a big "and" of: for every tuple T in lifted-upper-bounds: (T in leftE) implies (T in rightE)
     #;(node/formula/op/&& (length lifted-upper-bounds) ; List<Tuples>                       
                         (map (lambda (x)
                                (define ante (node/formula/op/in (tuple2Expr x) leftE))
                                (define cons (node/formula/op/in (tuple2Expr x) rightE))
                                (node/formula/op/=> 2 ante cons)) lifted-upper-bounds))
    ; = (atomic fmla)
    ; Q: How do we know that this is an expression? I changed it to call desugar-formula recursively
    (node/formula/op/in (list leftE rightE)) ; placeholder
    ]

    ; EQUALS 
    [(? node/formula/op/=?)
     (printf "=~n")
     ; The desugared version of equals should be LHS in RHS AND RHS in LHS 
     (let ([desugaredEquals (node/formula/op/&& (list
                                                  (node/formula/op/in (list (first args) (second args)))
                                                  (node/formula/op/in (list (second args) (first args)))))])
       ; Call desugar-formula recursively on the desugared expression created on the previous step 
       (desugar-formula desugaredEquals quantvars))
       ;(map (lambda (x) (desugar-expr x quantvars)) desugaredEquals))
     ]

    ; NEGATION
    [(? node/formula/op/!?)
     (printf "not~n")
     (let ([desugaredNegation (node/formula/op/=> (list (first args) #f))])
       (desugar-formula desugaredNegation quantvars))] 
     ; (map (lambda (x) (desugar-formula x quantvars)) args)

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

(define (desugar-expr expr quantvars)
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
     ; CurrentTuppleIfAtomic should be the implicit LHS of the expression, so therefore the first element in args 
     (desugar-expr-op expr quantvars args (car args))]

    ; Q: I am a little bit confused about this case 
    ; quantified variable (depends on scope! which quantifier is this var for?)
    [(node/expr/quantifier-var arity sym)     
     ;;(print-cmd-cont (symbol->string (v (get-var-idx expr quantvars))))
     (printf "  ~a~n" sym)]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension len decls form)     
     (define vars (map car decls)) ; account for multiple variables  
     (let ([quantvars (append vars quantvars)])       
       (printf "comprehension over ~a~n" vars)
       (for-each (lambda (d) ; each declaration
                   ;(print-cmd-cont (format "[~a : " (v (get-var-idx (car d) quantvars))))
                   (desugar-expr (cdr d) quantvars)
                   (printf "    decl: ~a~n" d))
                 decls)     
       (desugar-formula form quantvars))]))

(define (desugar-expr-op expr quantvars args currTupIfAtomic)
  (match expr
    ; union
    [(? node/expr/op/+?)
     (printf "+~n")
     ; Q: Should I be accounting for multiple unions? like in the intersection case below? 
     ; The desugared version of Union should be currentTupleIfAtomic in LHS OR currentTupleIfAtomic in RHS
     (define currTupIfAtomicExpr (tuple2Expr currTupIfAtomic))
     (let ([desugared1 (node/formula/op/in (list currTupIfAtomicExpr (first args)))])
       (let ([desugared2 (node/formula/op/in (list currTupIfAtomicExpr (second args)))])
         (let ([desugaredUnion (node/formula/op/|| (list desugared1 desugared2))])
           (desugar-formula desugaredUnion quantvars))))
     ;(map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; setminus
    [(? node/expr/op/-?)
     (printf "-~n")
      ; The desugared version of SetMinus should be currentTupleIfAtomic in LHS AND not(currentTupleIfAtomic in RHS)
     (define currTupIfAtomicExpr (tuple2Expr currTupIfAtomic))
     (let ([desugared1 (node/formula/op/in (list currTupIfAtomicExpr (first args)))])
       (let ([desugared2 (node/formula/op/! (list node/formula/op/in (list currTupIfAtomicExpr (second args))))])
         (let ([desugaredSetMinus (node/formula/op/&& (list desugared1 desugared2))])
           (desugar-formula desugaredSetMinus quantvars))))
     ;(map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; intersection
    [(? node/expr/op/&?)
     ;(printf "& ~a~n" expr)
     (define children (map (lambda (x) (desugar-expr x quantvars)) args))
     ; first argument of & struct is the arity, second is the child expressions
     ; Q: Why are we creating a new intersection here? 
     (node/expr/op/& (length children) children)
     ]
    ; product
    [(? node/expr/op/->?)
     (printf "->~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; join
    [(? node/expr/op/join?)
     (printf ".~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; transitive closure
    [(? node/expr/op/^?)
     (printf "^~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; reflexive-transitive closure
    [(? node/expr/op/*?)
     (printf "*~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; transpose
    [(? node/expr/op/~?)
     (printf "~~~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; singleton (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]))

(define (desugar-int expr quantvars)
  (match expr
    ; constant int
    [(node/int/constant value)
     (printf "~a~n" value)]
    ; apply an operator to some integer expressions
    [(node/int/op args)   
     (desugar-int-op expr quantvars args)]
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age  
    [(node/int/sum-quant decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ;( print-cmd-cont (format "(sum ([~a : ~a " 
       ;                         (v (get-var-idx var quantvars))
       ;                         (if (@> (node/expr-arity var) 1) "set" "one")))
       (desugar-expr (cdr (car decls)) quantvars)
       
       (desugar-int int-expr quantvars)
       )]))

(define (desugar-int-op expr quantvars args)
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
     (map (lambda (x) (desugar-expr x quantvars )) args)
     ]
    ; cardinality (e.g., #Node)
    [(? node/int/op/card?)
     (printf "cardinality~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
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

; Function tuple2Expression which turns a Tuple into an Expression
(define (tuple2Expr tup)
  (tup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Node  (declare-relation '(univ) 'univ "Node"))
(define edges (declare-relation '(Node Node) 'Node "edges"))
(define f-symmetric (= edges (~ edges)))
(define f-irreflexive (no (& edges iden)))
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

"Symmetric ~n" 
(desugar-formula f-symmetric '())
"Irreflexive ~n" 
(desugar-formula f-irreflexive '())
"some-reaches-all ~n" 
(desugar-formula f-some-reaches-all '())



