#lang forge/core
(require (prefix-in @ racket))

; Bounds lifting functions for Amalgam
; expression x bounds -> bounds

; Every Kodkod problem has (upper, lower) bounds for every relation.
; Amalgam needs to have safe bounds estimates for every *expression*
;  since I might write some x : A+B, I need to know the upper-bound for
;   A+B in order to turn the quantified formula into a big "or".
; One thing we might do is just use univ (of appropriate arity), but
;  that gets very large and unwieldy quite quickly. E.g.,
;    suppose UB(A) = {A0, A1}, UB(B) = {B0, B1},
;            UB(C) = {C0, C1, C2}, UB(Int) = [-8, ..., 7].
;   Then if we convert the above quantified formula using "univ", we'll build
;    a big "or" with *23* disjuncts, rather than the *4* needed (note C wasn't
;    included).
;  Also, the desugaring algorithm uses upperBounds in a lot of other places,
;    e.g., "R in Q" becomes a big "and" saying that all possible members of R
;    are in Q (if they are in R).
;
;  We therefore need this function to "lift" the notion of bounds on a relation
;   to arbitrary expressions.

; Adapted from original Amalgam UpperBoundVisitor.java at:
; https://github.com/transclosure/amalgam/blob/master/src/edu/mit/csail/sdg/
; alloy4compiler/translator/AmalgamUpperBoundVisitor.java

(provide liftBoundsExpr)
(require "lift-bounds_helpers.rkt")
(require debug/repl)

; input: expr - the expression that we are trying to get the bounds of 
;        quantVars - the quantifiable variables of the expression
;        runContext - the run context of the current program being ran 
;
; output: Returns a list of tuples (list <tuple>) containing the bounds
; of the given expression. 
(define/contract (liftBoundsExpr expr quantvars runContext)
  (@-> node/expr? list? forge:Run? (listof (or/c (listof number?) (listof symbol?))))
  (match expr

    ; atom case (base case)
    [(node/expr/atom info arity name)
     (define tuple (list (node/expr/atom-name expr)))
     (list tuple)]
    
    ; relation name (base case)
    [(node/expr/relation info arity name typelist parent isvar)
     (define allBounds
       (forge:Run-kodkod-bounds runContext))    
     (define filteredBounds
       (filter (lambda (b)
                 (equal? name
                         (forge:relation-name (forge:bound-relation b))))
               allBounds))
     (cond [(equal? (length filteredBounds) 1)
            (forge:bound-upper (first filteredBounds))]
           [else (error (format
                         "liftBoundsExpr on ~a: didn't have a bound for ~a in ~a"
                         expr name allBounds))])]

    ; The Int constant
    [(node/expr/constant info 1 'Int)
     (define allBounds
       (forge:Run-kodkod-bounds runContext)) 
     (define filteredBounds
       (filter (lambda (b)
                 (equal? "Int"
                         (forge:relation-name (forge:bound-relation b))))
               allBounds))
     (cond [(equal? (length filteredBounds) 1)
            (forge:bound-upper (first filteredBounds))]
           [else
            (error
             (format "liftBoundsExpr on ~a: didn't have a bound for ~a in ~a"
                     expr "Int" allBounds))])]

    ; other expression constants
    [(node/expr/constant info arity type)
     (cond
       [(equal? type 'univ) (map (lambda (x) (list x x))
                                 (forge:Run-atoms runContext))]
       [(equal? type 'iden) (map (lambda (x) (list x x))
                                 (forge:Run-atoms runContext))]
       [(equal? type 'none) '()])]
    
    ; expression w/ operator (union, intersect, ~, etc...)
    [(node/expr/op info arity args)
     (liftBoundsExprOp expr quantvars args runContext)]
    
    ; quantified variable
    [(node/expr/quantifier-var info arity sym)
     (error (format
             "We should not be getting the bounds of a quantified variable ~a"
             sym))]
    
    ; set comprehension e.g. {n : Node | some n.edges}
    [(node/expr/comprehension info len decls form)
     
     (define vars (map car decls)) 
     (let ([quantvars (append vars quantvars)])             
       ; {x: e1, y: e2 | ...}
       ; then UB(e1)->UB(e2) is the UB of the whole comprehension
       (define uppers
         (map (lambda (d)                                    
                (liftBoundsExpr (cdr d) quantvars runContext)) decls))
       (map (lambda (ub) (apply append ub)) (apply cartesian-product uppers)))]))

; input: expr - the expression that we are trying to get the bounds of 
;        quantVars - the quantifiable variables of the expression
;        args - the args (children) of the current expression that we're getting
;               the bounds of. 
;        runContext - the run context of the current program being ran 
;
; output: Returns a list of tuples (list <tuple>) containing the bounds
; of the given expression. 
(define (liftBoundsExprOp expr quantvars args runContext)
  (match expr

    ; SET UNION 
    [(? node/expr/op/+?)
     ; UB(LHS and RHS) = UB(LHS) + UB(RHS) 
     (define uppers 
       (map (lambda (arg)
              (liftBoundsExpr arg quantvars runContext)) args))
     (remove-duplicates (apply append uppers))]
    
    ; SET MINUS 
    [(? node/expr/op/-?)
     ; UB(A-B) = UB(A).
     (liftBoundsExpr (first args) quantvars runContext)]

    ; SET INTERSECTION
    [(? node/expr/op/&?)
     (define upperBounds
       (map (lambda (arg)
              (liftBoundsExpr arg quantvars runContext)) args))
     ; filter to filter out the LHS only if they are also in upper bounds of RHS
     (filter (lambda (x) (member x (first upperBounds)))
             (apply append (rest upperBounds)))]

    ; PRODUCT
    [(? node/expr/op/->?)
     ; UB(A->B) = UB(A) x UB(B)
     (define uppers 
       (map (lambda (arg)
              (liftBoundsExpr arg quantvars runContext)) args))
     (map (lambda (ub) (apply append ub)) (apply cartesian-product uppers))]

    ; JOIN
    [(? node/expr/op/join?)
     ; In order to approach a join with n arguments, we will first do a
     ; binary join and procede with a foldl doing a join on the previous
     ; result of the function
     (cond
       [(@< (node/expr-arity expr) 1)
        (error (format "Join was given expr ~a with arity less than 1" expr))]
       [else
        (define uppers 
          (map (lambda (arg)
                 (liftBoundsExpr arg quantvars runContext)) args))
        ; Note: assumes certain direction of associativity
        (define newTuples (joinTuple (first uppers) (second uppers)))
        (foldl (lambda (curr acc) (joinTuple acc curr))
               newTuples (rest (rest uppers)))])]

    ; TRANSITIVE CLOSURE
    [(? node/expr/op/^?)
     (buildClosureOfTupleSet (liftBoundsExpr (first args) quantvars runContext))]

    ; REFLEXIVE-TRANSITIVE CLOSURE 
    [(? node/expr/op/*?)
     (define closure (buildClosureOfTupleSet
                      (liftBoundsExpr (first args) quantvars runContext)))
     ; We remove duplicates before we are appending 'iden
     (remove-duplicates
      (append closure (map (lambda (x) (list x x))
                           (forge:Run-atoms runContext))))]

    ; TRANSPOSE 
    [(? node/expr/op/~?)
     (define upperBounds
       (map (lambda (x) (liftBoundsExpr x quantvars runContext)) args))
     ; flip the tuples in the upper bounds
     (map (lambda (x) (transposeTup x)) (first upperBounds))]

    ; SINGLETON (typecast number to 1x1 relation with that number in it)
    [(? node/expr/op/sing?)
     (liftBoundsInt (first args) quantvars runContext)]))

; input: expr - the expression that we are trying to get the bounds of 
;        quantVars - the quantifiable variables of the expression
;        runContext - the run context of the current program being ran 
;
; output: Returns a list of tuples (list <tuple>) containing the bounds
; of the given expression. 
(define (liftBoundsInt expr quantvars runContext)
  (match expr

    ; constant int
    [(node/int/constant info value)
     (define allBounds (forge:Run-kodkod-bounds runContext)) 
     (define filteredBounds (filter
                             (lambda (b) (equal? "Int"
                                                 (forge:relation-name
                                                  (forge:bound-relation b))))
                             allBounds))
     (cond [(equal? (length filteredBounds) 1)
            (forge:bound-upper (first filteredBounds))]
           [else (error
                  (format
                   "liftBoundsExpr on ~a: didn't have a bound for ~a in ~a"
                   expr "Int" allBounds))])]
    
    ; apply an operator to some integer expressions
    [(node/int/op info args)
     (liftBoundsIntOp expr quantvars args runContext)]
    
    ; sum "quantifier"
    [(node/int/sum-quant info decls intExpr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (liftBoundsExpr (node/expr/constant info 1 'Int)
                       quantvars runContext))]))

; input: expr - the expression that we are trying to get the bounds of 
;        quantVars - the quantifiable variables of the expression
;        runContext - the run context of the current program being ran 
;
; output: Returns mostly errors since int operators are not supported in
; Amalgam. 
(define (liftBoundsIntOp expr quantvars args runContext)
  (match expr
    ; int addition
    [(? node/int/op/add?)
     (error "amalgam: int + not supported")]
    
    ; int subtraction
    [(? node/int/op/subtract?)
     (error "amalgam: int - not supported")]
    
    ; int multiplication
    [(? node/int/op/multiply?)
     (error "amalgam: int * not supported")]
    
    ; int division
    [(? node/int/op/divide?)
     (error "amalgam: int / not supported")]
    
    ; int sum (also used as typecasting from relation to int)
    ; e.g. {1} --> 1 or {1, 2} --> 3
    [(? node/int/op/sum?)
     (error "amalgam: int sum not supported")]
    
    ; cardinality (e.g., #Node)
    [(? node/int/op/card?)
     (define allBounds
       (forge:Run-kodkod-bounds runContext))
     (define filteredBounds (filter
                             (lambda (b)
                               (equal? "Int"
                                       (forge:relation-name
                                        (forge:bound-relation b))))
                             allBounds))
     (cond [(equal? (length filteredBounds) 1)
            (forge:bound-upper (first filteredBounds))]
           [else (error (format
                         "liftBoundsExpr on ~a: didn't have a bound for ~a in ~a"
                         expr "Int" allBounds))])]
    
    ; remainder/modulo
    [(? node/int/op/remainder?)
     (error "amalgam: int % (modulo) not supported")]
    
    ; absolute value
    [(? node/int/op/abs?)
     (error "amalgam: int abs not supported")]
    
    ; sign-of 
    [(? node/int/op/sign?)
     (error "amalgam: int sign not supported")]))


