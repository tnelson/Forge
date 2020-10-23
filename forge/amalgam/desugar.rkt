#lang racket

; Desugaring functions for Amalgam
; (full AST) -> (restricted AST without stuff like implies)
;    Note: These functions maintain an environment of
;    quantified variables to aid general functionality

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Warning: ast.rkt exports (e.g.) "and".
; This is the macro that produces an "and" formula!
; To use real Racket and, use @and.
(require "../lang/ast.rkt" (prefix-in @ racket))

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugar-formula formula quantvars)
  (match formula
    ; Constant formulas
    [(node/formula/constant type)
     (printf "constant ~a~n" type)]
    ; operator formula (and, or, implies, ...)
    [(node/formula/op args)
     (desugar-formula-op formula quantvars args)]
    ; multiplicity formula (some, one, ...)
    [(node/formula/multiplicity mult expr)
     (desugar-expr expr quantvars)
     (printf "multiplicity ~a~n" mult expr)]
    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified quantifier decls form)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])      
       (desugar-expr (cdr (car decls)) quantvars)     
       (desugar-formula form quantvars)
       (printf "quant ~a~n" quantifier))]
    ; truth and falsity
    ; TODO: wat
    [#t (printf "true~n")]
    [#f (printf "false~n")]
    ))

(define (desugar-formula-op formula quantvars args)
  (match formula
    ; ? <test> matches on forms that <test> returns true for

    ; AND
    [(? node/formula/op/&&?)
     (printf "and~n")
     (map (lambda (x) (desugar-formula x quantvars)) args)
     ]
    ; OR
    [(? node/formula/op/||?)
     (printf "or~n")
     (map (lambda (x) (desugar-formula x quantvars)) args)
     ]
    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "implies~n")
     (map (lambda (x) (desugar-formula x quantvars)) args)
     ]
    ; IN (atomic fmla)
    [(? node/formula/op/in?)
     (printf "in~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; = (atomic fmla)
    [(? node/formula/op/=?)
     (printf "=~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]

    ; NEGATION
    [(? node/formula/op/!?)
     (printf "not~n")
     (map (lambda (x) (desugar-formula x quantvars)) args)
     ]
    ; INTEGER >
    [(? node/formula/op/int>?)
     (printf "int>~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
    ]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (printf "int<~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (printf "int=~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]))

(define (desugar-expr expr quantvars)  
  (match expr
    ; relation name (base case)
    [(node/expr/relation arity name typelist parent)
     (printf "relation ~a~n" name)]
    ; The Int constant
    [(node/expr/constant 1 'Int)
     (printf "Int~n")]
    ; other expression constants
    [(node/expr/constant arity type)
     (printf "constant ~a~n" type)]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op arity args)
     (desugar-expr-op expr quantvars args)]
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
                   ; car = first thing; cdr = remainder
                   (desugar-expr (cdr d) quantvars)
                   (printf "    decl: ~a~n" d))
                 decls)     
       (desugar-formula form quantvars))]))

(define (desugar-expr-op expr quantvars args)
  (match expr
    ; union
    [(? node/expr/op/+?)
     (printf "+~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; setminus
    [(? node/expr/op/-?)
     (printf "-~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
     ]
    ; intersection
    [(? node/expr/op/&?)
     (printf "&~n")
     (map (lambda (x) (desugar-expr x quantvars)) args)
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
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ; int subtraction
    [(? node/int/op/subtract?)
     (printf "int-~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ; int multiplication
    [(? node/int/op/multiply?)
     (printf "int*~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ; int division
    [(? node/int/op/divide?)
     (printf "int/~n")
     (map (lambda (x) (desugar-int x quantvars )) args)
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
     (printf "remainder~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ; absolute value
    [(? node/int/op/abs?)
     (printf "abs~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ; sign-of 
    [(? node/int/op/sign?)
     (printf "sign~n")
     (map (lambda (x) (desugar-int x quantvars)) args)
     ]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Node  (declare-relation '(univ) 'univ "Node"))
(define edges (declare-relation '(Node Node) 'Node "edges"))
(define f-symmetric (= edges (~ edges)))

(desugar-formula f-symmetric '())



