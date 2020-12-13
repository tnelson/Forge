#lang forge/core

; This recursive tree is meant to act as a substitutor from variables
; to a given value. If given a formula, this tree is going to return a
; formula, and if given an expression, this tree is going to return an
; expression.

; IMPORTANT NOTE: This tree DOES NOT take into account the 'meaning'
; behind the things that it is substituting, it merely recurs on
; the children of a given formula/expression and puts the children
; together with the original operator that was identified. 

(provide substituteFormula toStringSub)
(require debug/repl)
(require (prefix-in @ (only-in racket ->)))
;;;;;;;;;;;;;;;;;;;;;;;;;

; input: formula - the formula we are substituting in
;        quantvars - quantvars passed in from original substitute tree
;        target - the value that value should be replacaed with, an expression
;        value - the value that we are looking to replace, an expression
;
; output: the formula with target substituted by value
(define/contract (substituteFormula formula quantvars target value)
  (@-> node/formula? list? node/expr? node/expr? node/formula?)
  (match formula
    ; Constant formulas: already at bottom
    [(node/formula/constant info type)
     (cond 
           [(equal? (toStringSub formula) (toStringSub target)) value]
           [(not (equal? (toStringSub formula) (toStringSub target))) formula])]

    ; operator formula (and, or, implies, ...)
    [(node/formula/op info args)
     (substituteFormulaOp formula quantvars args info target value)]
    
    ; multiplicity formula (some, one, ...) 
    [(node/formula/multiplicity info mult expr)
      (multiplicity-formula info mult
                            (substituteExpr expr quantvars target value))]

    ; quantified formula (some x : ... or all x : ...)
    ; decls: ([n1 Node] [n2 Node] [c City.edges])
    [(node/formula/quantified info quantifier decls subform)
     ; might be multiple variables in one quantifier e.g. some x1, x2: Node | ...
     (define vars (map car decls))
     ; error checking
     (for-each
      (lambda (qv)
        (when (equal? (toStringSub qv) (toStringSub target))
          (error
           (format
            "substitution encountered quantifier that shadows substitution target ~a"
            target)))
        (when (member qv quantvars)
          (error
           (format "substitution encountered shadowed quantifier ~a" qv)))) vars)
     (let ([quantvars (append vars quantvars)])       
       (quantified-formula
        info quantifier
        (map (lambda (decl)
               (cons (car decl)
                     (substituteExpr (cdr decl) quantvars target value))) decls)
                           (substituteFormula subform quantvars target value)))]    
    [else (error (format "no matching case in substitution for ~a" formula))]))

; input: formula - the formula we are substituting in
;        quantvars - quantvars passed in from original substitute tree
;        args - a list of the argments of the expression passed in
;        info - info of the original node
;        target - the value that value should be replacaed with, an expression
;        value - the value that we are looking to replace, an expression
;
; output: the formula with target substituted by value
(define/contract (substituteFormulaOp formula quantvars args info target value)
  (@-> (or/c node/formula? node/expr?) list?
       (or/c (listof node/formula?) (listof node/expr?) (listof symbol?))
       nodeinfo? (or/c node/expr? node/formula?) (or/c node/expr? node/formula?)
       (or/c node/expr? node/formula?))     
  (match formula

    ; AND 
     [(? node/formula/op/&&?)
     (define substitutedArgs
       (map (lambda (x) (substituteFormula x quantvars target value)) args))
     (node/formula/op/&& info substitutedArgs)]

    ; OR
     [(? node/formula/op/||?)
     (define substitutedArgs
       (map (lambda (x) (substituteFormula x quantvars target value)) args))
     (node/formula/op/|| info substitutedArgs)]

    ; IMPLIES
    [(? node/formula/op/=>?)
     (define substitutedLHS
       (substituteFormula  (first args) quantvars target value))
     (define substitutedRHS
       (substituteFormula  (second args) quantvars target value))
     (node/formula/op/=> info (list substitutedLHS substitutedRHS))]

    ; IN (atomic fmla)
    [(? node/formula/op/in?)
     (define substitutedLHS
       (substituteExpr (first args) quantvars target value))
     (define substitutedRHS
       (substituteExpr (second args) quantvars target value))

     ;;;; temporary debugging check
     ;;;; ideally, this would be a "is the input type the same as the output type?" contract
     (unless (node/expr? substitutedLHS)
       (error "IN subst produced LHS: ~a ~a~n" (first args) substitutedLHS))
     (unless (node/expr? substitutedRHS)
       (error "IN subst produced RHS: ~a ~a~n" (second args) substitutedRHS))
     (node/formula/op/in info (list substitutedLHS substitutedRHS))]

    ; EQUALS 
    [(? node/formula/op/=?)
     (define substitutedLHS
       (substituteExpr  (first args) quantvars target value))
     (define substitutedRHS
       (substituteExpr  (second args) quantvars target value))
     (node/formula/op/= info (list substitutedLHS substitutedRHS))]

    ; NEGATION
    [(? node/formula/op/!?)
     (define substitutedEntry
       (substituteFormula (first args) quantvars target value))
     (node/formula/op/! info (list substitutedEntry))]   

    ; INTEGER >
    [(? node/formula/op/int>?)
     (error "amalgam: int > not supported ~n")]
    ; INTEGER <
    [(? node/formula/op/int<?)
     (error "amalgam: int < not supported ~n")]
    ; INTEGER =
    [(? node/formula/op/int=?)
     (error "amalgam: int = not supported ~n")]))

; input: expr - the expression we are substituting in 
;        quantvars - quantvars passed in from original substitute tree
;        target - the value that value should be replacaed with
;        value - the value that we are looking to replace
;
; output: the expression with target substituted by value
(define/contract (substituteExpr expr quantvars target value)
  (@-> node/expr? list? node/expr? node/expr? node/expr?)
  (match expr

    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent isvar)
       (cond 
         [(equal? (toStringSub expr) (toStringSub target)) value]
         [(not(equal? (toStringSub expr) (toStringSub target))) expr])]

    ; atom (base case)
    [(node/expr/atom info arity name)
     (cond 
       [(equal? (toStringSub expr) (toStringSub target)) value]
       [(not(equal? (toStringSub expr) (toStringSub target))) expr])]

    ; The INT Constant
    [(node/expr/constant info 1 'Int)
       (cond 
         [(equal? (toStringSub expr) (toStringSub target)) value]
         [(not(equal? (toStringSub expr) (toStringSub target))) expr])]

    ; other expression constants
    [(node/expr/constant info arity type)
       (cond 
         [(equal? (toStringSub expr) (toStringSub target)) value]
         [(not(equal? (toStringSub expr) (toStringSub target))) expr])]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (substituteExprOp expr quantvars args info target value)]
 
    ; quantified variable (depends on scope!)
    ; (another base case)
    [(node/expr/quantifier-var info arity sym)
     (cond  [(equal? (toStringSub expr) (toStringSub target)) value]
            [(not (equal? (toStringSub expr) (toStringSub target))) expr])]

    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info len decls subform)
      ; account for multiple variables  
     (define vars (map car decls))
     (for-each
      (lambda (v)
        (when (equal? (toStringSub v) (toStringSub target))
          (error
           (format
            "substitution encountered quantifier that shadows substitution target ~a" target)))
        (when (member v quantvars)
          (error (format "substitution encountered shadowed quantifier ~a" v))))
                 vars)
     (let ([quantvars (append vars quantvars)])               
       (comprehension
        info
        (map (lambda (decl)
               (cons (car decl)
                     (substituteExpr (cdr decl) quantvars target value))) decls)
                      (substituteFormula subform quantvars target value)))]

    [else (error (format "no matching case in substitution for ~a" expr))]))

; input: expr - the expression we are substituting in 
;        quantvars - quantvars passed in from original substitute tree
;        args - a list of the argments of the expression passed in
;        info - info of the original node
;        target - the value that value should be replacaed with
;        value - the value that we are looking to replace
;
; output: the expression with target substituted by value
(define/contract (substituteExprOp expr quantvars args info target value)
  (@-> node/expr/op? list?
       (or/c (listof node/formula?) (listof node/expr?) (listof symbol?))
       nodeinfo? node/expr? node/expr? node/expr/op?)
  (match expr

    ; UNION
    [(? node/expr/op/+?)
     ; map over all children of union
     (define substitutedChildren
       (map
        (lambda (child) (substituteExpr child quantvars target value)) args))
     (node/expr/op/+ info (node/expr-arity expr) substitutedChildren)]
    
    ; SETMINUS 
    [(? node/expr/op/-?)
     (cond
       [(not(equal? (length args) 2))
        (error("Setminus should not be given more than two arguments ~n"))]
       [else 
        (define LHS (substituteExpr (first args) quantvars target value))
        (define RHS (substituteExpr (second args) quantvars target value))
        (node/expr/op/- info (node/expr-arity expr) (list LHS RHS))])]
    
    ; INTERSECTION
    [(? node/expr/op/&?)
     ; map over all children of intersection
     (define substitutedChildren
       (map
        (lambda (child) (substituteExpr child quantvars target value)) args))
     (node/expr/op/& info (node/expr-arity expr) substitutedChildren)]
    
    ; PRODUCT
    [(? node/expr/op/->?)
     ; map over all children of product
     (define substitutedChildren
       (map
        (lambda (child) (substituteExpr child quantvars target value)) args))
     (node/expr/op/-> info (node/expr-arity expr) substitutedChildren)]
   
    ; JOIN
    [(? node/expr/op/join?)
     ; map over all children of join
     (define substitutedChildren
       (map
        (lambda (child) (substituteExpr child quantvars target value)) args))
     (node/expr/op/join info (node/expr-arity expr) substitutedChildren)]
    
    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (define substitutedChildren
       (map
        (lambda (child) (substituteExpr child quantvars target value)) args))
     (node/expr/op/^ info (node/expr-arity expr) substitutedChildren)]
    
    ; REFLEXIVE-TRANSITIVE CLOSURE
    [(? node/expr/op/*?)
     (define substitutedChildren
       (map
        (lambda (child) (substituteExpr child quantvars target value)) args))
     (node/expr/op/* info (node/expr-arity expr) substitutedChildren)]
    
    ; TRANSPOSE
    [(? node/expr/op/~?)
     (define substitutedEntry
       (substituteExpr (first args) quantvars target value))
     (node/expr/op/~ info (node/expr-arity expr) (list substitutedEntry))]
    
    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (define substitutedEntry
       (substituteInt (first args) quantvars target value))
     (node/expr/op/sing info (node/expr-arity expr) (list substitutedEntry))]))


; input: expr - the expression we are substituting in 
;        quantvars - quantvars passed in from original substitute tree
;        target - the value that value should be replacaed with
;        value - the value that we are looking to replace
;
; output: the expression with target substituted by value
(define/contract (substituteInt expr quantvars target value)
  (@-> node/int? list? node/expr? node/expr? node/int?)
  (match expr
    
    ; CONSTANT INT
    [(node/int/constant info intValue)
       (cond 
         [(equal? (toStringSub expr) (toStringSub target)) value]
         [(not(equal? (toStringSub expr) (toStringSub target))) expr])]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)
     (substituteIntOp expr quantvars args info target value)]
    
    ; sum "quantifier"
    ; e.g. sum p : Person | p.age
    [(node/int/sum-quant info decls int-expr)
      ; account for multiple variables  
     (define vars (map car decls))
     (for-each
      (lambda (v)
        (when (equal? (toStringSub v) (toStringSub target))
          (error
           (format
            "substitution encountered quantifier that shadows substitution target ~a"
            target)))
        (when (member v quantvars)
          (error (format "substitution encountered shadowed quantifier ~a" v))))
      vars)
     (let ([quantvars (append vars quantvars)])
       (sum-quant-expr info
                      (map (lambda (decl)
                             (cons (car decl)
                                   (substituteExpr
                                    (cdr decl) quantvars target value))) decls)
                      (substituteInt int-expr quantvars target value)))]))


; input: expr - the expression we are substituting in 
;        quantvars - quantvars passed in from original substitute tree
;        args - a list of the argments of the expression passed in
;        info - info of the original node
;        target - the value that value should be replacaed with
;        value - the value that we are looking to replace
;
; output: because amalgam does not support int operators, this will
;         yield an error in all cases except for the cardinality case
;
; This helper is used for transitive and reflexive transtivie closure.
;   Helper used to build a closure of tuple sets.
(define (substituteIntOp expr quantvars args info target value)
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
     (define substitutedEntry
       (substituteExpr (first args) quantvars target value))
     (node/int/op/card info (list substitutedEntry))]  
    
    ; remainder/modulo
    [(? node/int/op/remainder?)     
     (error "amalgam: int % (modulo) not supported~n")]
    
    ; absolute value
    [(? node/int/op/abs?)
     (error "amalgam: int abs not supported~n")]
    
    ; sign-of 
    [(? node/int/op/sign?)
     (error "amalgam: int sign-of not supported~n")]))


; input: x - the expression/formula to be converted
;
; output: string version of x
;
; Function to translate the value and expression/formula to string to check for
; equality. Without translating to a string, we can't check for equality
; given that the info field makes them different.
(define (toStringSub x)
  (format "~v" x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



