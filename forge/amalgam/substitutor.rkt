#lang forge/core

; This recursive tree is meant to act as a substitutor from variables
; to a given value. If given a formula, this tree is going to return a
; formula, and if given an expression, this tree is going to return an
; expression.

; IMPORTANT NOTE: This tree DOES NOT take into account the 'meaning'
; behind the things that it is substituting, it merely recurs on
; the children of a given formula/expression and puts the children
; together with the original operator that was identified. 

(provide substitute-formula)

;;;;;;;;;;;;;;;;;;;;;;;;;

(define (substitute-formula formula quantvars variable value)
  (match formula
    ; Constant formulas: already at bottom   
    [(node/formula/constant info type)
     (cond (
           [(equal? formula variable) value]
           [(not(equal? formula variable)) variable]
           ))]

    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     (substitute-formula-op formula quantvars args info variable value)]
    
    ; multiplicity formula (some, one, ...) 
    [(node/formula/multiplicity info mult expr)
      (multiplicity-formula info mult (substitute-expr expr quantvars variable value))]

    ; quantified formula (some x : ... or all x : ...)
    ; decls: ([n1 Node] [n2 Node] [c City.edges])
    [(node/formula/quantified info quantifier decls subform)
     ; might be multiple variables in one quantifier e.g. some x1, x2: Node | ...
     (define vars (map car decls))
     ; error checking
     (for-each (lambda (qv)
                 (when (equal? qv variable)
                   (error (format "substitution encountered quantifier that shadows substitution variable ~a" variable)))
                 (when (member qv quantvars)
                   (error (format "substitution encountered shadowed quantifier ~a" qv))))
                 vars)

     (let ([quantvars (append vars quantvars)])       
       (quantified-formula info quantifier
                           (map (lambda (decl)
                                  (cons (car decl) (substitute-expr (cdr decl) quantvars variable value))) decls)
                           (substitute-formula subform quantvars variable value)))]
        
    [else (error (format "no matching case in substitution for ~a" formula))]))

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
     ; TODO: is this substitute-formula or substitute-expr?
     (define substitutedLHS (substitute-formula  (first args) quantvars variable value))
     (define substitutedRHS (substitute-formula  (second args) quantvars variable value))
     (node/formula/op/in info (list substitutedLHS substitutedRHS))]

    ; EQUALS 
    [(? node/formula/op/=?)
     (printf "=~n")
     ; TODO: same as above
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

(define (substitute-expr expr quantvars variable value)
  ; Error message to check that we are only taking in expressions
  (unless (node/expr? expr) (error (format "substitute-expr called on non-expr: ~a" expr)))

  (match expr

    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent)
       (cond (
         [(equal? expr variable) value]
         [(not(equal? expr variable)) variable]
        ))]

    ; The INT Constant
    [(node/expr/constant info 1 'Int)
       (cond (
         [(equal? expr variable) value]
         [(not(equal? expr variable)) variable]
        ))]

    ; other expression constants
    [(node/expr/constant info arity type)
       (cond (
         [(equal? expr variable) value]
         [(not(equal? expr variable)) variable]
        ))]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (substitute-expr-op expr quantvars args info variable value)]
 
    ; quantified variable (depends on scope!)
    ; (another base case)
    [(node/expr/quantifier-var info arity sym)     
     (cond ([(equal? expr variable) value]
            [(not (equal? expr variable)) variable]))]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info len decls subform)
      ; account for multiple variables  
     (define vars (map car decls))
     (let ([quantvars (append vars quantvars)])       
       (printf "comprehension over ~a~n" vars)

       ; TODO: add safety checks as in quantifier case
              
       (comprehension info
                      (map (lambda (decl)
                             (cons (car decl) (substitute-expr (cdr decl) quantvars variable value))) decls)
                      (substitute-formula subform quantvars variable value)))]

; TODO: add defensive error case!
    ))

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
     (node/expr/op/& info substitutedChildren)]
    
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
     (define substitutedEntry (substitute-expr (first args) quantvars variable value))
     (node/expr/op/~ info (list substitutedEntry))]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (printf "sing~n")
     (define substitutedEntry (substitute-expr (first args) quantvars variable value))
     (node/expr/op/sing info (list substitutedEntry))]))

(define (substitute-int expr quantvars variable value)
  (match expr
    
    ; CONSTANT INT
    [(node/int/constant info intValue)
       (cond (
         [(equal? expr variable) value]
         [(not(equal? expr variable)) variable]
        ))]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)   
     (substitute-int-op expr quantvars args info variable value)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age
    ; TODO: finisht his case
    [(node/int/sum-quant info decls int-expr)
     (printf "sumQ~n")
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (substitute-expr (cdr (car decls)) quantvars variable value)
       (substitute-int int-expr quantvars variable value)
       )]))

(define (substitute-int-op expr quantvars args info variable value)
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
     (node/int/op/card info (list substitutedEntry))]  
    
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



