#lang forge/core

; This recursive tree is meant to act as a substitutor from variables
; to a given value. If given a formula, this tree is going to return a
; formula, and if given an expression, this tree is going to return an
; expression.

; IMPORTANT NOTE: This tree DOES NOT take into account the 'meaning'
; behind the things that it is substituting, it merely recurs on
; the children of a given formula/expression and puts the children
; together with the original operator that was identified. 

(require "desugar_helpers.rkt")
(provide substitute-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (substitute-formula formula quantvars variable value)
  (match formula
    ; TODO: FIX BASE CASES 
    ; Constant formulas: already at bottom
    [(node/formula/constant info type)
     formula]
    
    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     (substitute-formula-op formula quantvars args info variable value)]
    
    ; multiplicity formula (some, one, ...) 
    [(node/formula/multiplicity info mult expr)
      (define substituteedMultiplicity (node/formula/quantified info mult expr formula))
      (substitute-formula substituteedMultiplicity quantvars variable value)]

    
    ; quantified formula (some x : ... or all x : ...)
    [(node/formula/quantified info quantifier decls form)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (substitute-expr (cdr (car decls)) quantvars variable value)     
       (substitute-formula info form quantvars variable value)
       (printf "quant ~a~n" quantifier))]
    
    ; truth and falsity
    [#t (printf "true~n")]
    [#f (printf "false~n")]
    ))

(define (substitute-formula-op formula quantvars args info variable value)
  (match formula

    ; AND 
     [(? node/formula/op/&&?) 
     (printf "and~n")
     (define substitutedArgs (map (lambda (x) (substitute-formula x quantvars variable value)) args))
     (node/formula/op/&& info substitutedArgs)]

    ; OR
     [(? node/formula/op/||?)
     (printf "or~n")
     (define substitutedArgs (map (lambda (x) (substitute-formula x quantvars variable value)) args))
     (node/formula/op/|| info substitutedArgs)]

    ; IMPLIES
    [(? node/formula/op/=>?)
     (printf "implies~n")
     (define substitutedLHS (substitute-formula  (first args) quantvars variable value))
     (define substitutedRHS (substitute-formula  (second args) quantvars variable value))
     (node/formula/op/=> info (list substitutedLHS substitutedRHS))]

    ; IN (atomic fmla)
    [(? node/formula/op/in?)
     (printf "in~n")
     (define substitutedLHS (substitute-formula  (first args) quantvars variable value))
     (define substitutedRHS (substitute-formula  (second args) quantvars variable value))
     (node/formula/op/in info (list substitutedLHS substitutedRHS))]

    ; EQUALS 
    [(? node/formula/op/=?)
     (printf "=~n")
     (define substitutedLHS (substitute-formula  (first args) quantvars variable value))
     (define substitutedRHS (substitute-formula  (second args) quantvars variable value))
     (node/formula/op/= info (list substitutedLHS substitutedRHS))]

    ; NEGATION
    [(? node/formula/op/!?)
     (printf "not~n")
     (define substitutedEntry (substitute-formula (first args) quantvars variable value))
     (node/formula/op/! info (list substitutedEntry))]   

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
; TODO: change bsae cases
(define (substitute-expr expr quantvars variable value)
  ; Error message to check that we are only taking in expressions
  (unless (node/expr? expr) (error (format "substitute-expr called on non-expr: ~a" expr)))

  (match expr
    ; relation name (base case)
    [(and (node/expr/relation info arity name typelist parent) (equal? expr variable)) value]
    [(and (node/expr/relation info arity name typelist parent) (not (equal? expr variable))) variable]
    
    ; The Int constant
    [(and (node/expr/constant info 1 'Int) (equal? expr variable)) value]
    [(and (node/expr/constant info 1 'Int) (not (equal? expr variable))) variable]
    
    ; other expression constants
    [(and (node/expr/constant info arity type) (equal? expr variable)) value]
    [(and (node/expr/constant info arity type) (not (equal? expr variable))) variable]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (substitute-expr-op expr quantvars args info variable value)]
 
    ; quantified variable (depends on scope!)
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
                   (substitute-expr (cdr d) quantvars variable value)
                   (printf "    decl: ~a~n" d))
                 decls)     
       (substitute-formula form quantvars variable value))]))

(define (substitute-expr-op expr quantvars args info variable value)
  (match expr

    ; UNION
    [(? node/expr/op/+?)
     (printf "+~n")
     ; map over all children of union
     (define substitutedChildren
       (map
        (lambda (child) (substitute-expr child quantvars variable value)) args))
     (node/expr/op/+ info substitutedChildren)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (printf "-~n")
     (cond
       [(!(equal? (length args) 2)) (error("Setminus should not be given more than two arguments ~n"))]
       [else 
        (define LHS (substitute-expr (first args) quantvars variable value))
        (define RHS (substitute-expr (second args) quantvars variable value))
        (node/expr/op/- info (list LHS RHS))])]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     (printf "& ~a~n" expr)
     ; map over all children of intersection
     (define substitutedChildren
       (map
        (lambda (child) (substitute-expr child quantvars variable value)) args))
     (node/expr/op/&& info substitutedChildren)]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     (printf "->~n")
     ; map over all children of product
     (define substitutedChildren
       (map
        (lambda (child) (substitute-expr child quantvars variable value)) args))
     (node/expr/op/-> info substitutedChildren)]
   
    ; JOIN
    [(? node/expr/op/join?)
     (printf ".~n")
     ; map over all children of join
     (define substitutedChildren
       (map
        (lambda (child) (substitute-expr child quantvars variable value)) args))
     (node/expr/op/join info substitutedChildren)]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (printf "^~n")
     (define substitutedChildren
       (map
        (lambda (child) (substitute-expr child quantvars variable value)) args))
     (node/expr/op/^ info substitutedChildren)]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (printf "*~n")
     (define substitutedChildren
       (map
        (lambda (child) (substitute-expr child quantvars variable value)) args))
     (node/expr/op/* info substitutedChildren)]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (printf "~~~n")
     (define substitutedEntry (substitute-expression (first args) quantvars variable value))
     (node/formula/op/~ info (list substitutedEntry))]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (define substitutedEntry (substitute-expression (first args) quantvars variable value))
     (node/formula/op/sing info (list substitutedEntry))]))

(define (substitute-int expr quantvars variable value)
  (match expr
    ; CONSTANT INT
    ; TODO: change these base cases
    [(node/int/constant info value)
     (printf "~a~n" value)]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)   
     (substitute-int-op expr quantvars args variable value)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age
    ; TODO: finisht his case
    [(node/int/sum-quant info decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (substitute-expr (cdr (car decls)) quantvars variable value)
       (substitute-int int-expr quantvars runContext variable value)
       )]))

(define (substitute-int-op expr quantvars args variable value)
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
     (define substitutedEntry (substitute-formula (first args) quantvars variable value))
     (node/formula/op/card info (list substitutedEntry))]  
    
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



