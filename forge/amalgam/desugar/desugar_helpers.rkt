#lang forge/core
(require debug/repl)
(provide tup2Expr transposeTup mustHaveTupleContext isGroundProduct checkIfUnary checkIfBinary
         create-bitwidth-list createNewQuantifier projectTupleRange getColumnRight getColumnLeft)
(require "../lift-bounds/lift-bounds.rkt")
(require "../substitutor/substitutor.rkt")

; return list of lists inclusive of start and end
(define (create-bitwidth-list start end)
  (cond
    [(equal? start (+ end 1)) '()]
    [else
     (cons (list start) (create-bitwidth-list (+ 1 start) end))]))

; Helper to transform a given tuple from the lifted-upper bounds function to a relation, and then do the product of all relations
; to form an expression.
; <info> argument is optional; if not passed, will default to empty-nodeinfo
(define (tup2Expr tuple context info)
  (define tupRelationList
    ; replace every element of the tuple (atoms) with the corresponding atom relation
    (map
     (lambda (tupElem)
       ; keep only the atom relations whose name matches tupElem
      ;(define filterResult
      ;   (filter (lambda (atomRel)
       ;            (when (string? tupElem) (set! tupElem (string->symbol tupElem)))
       ;            (when (string? atomRel) (set! atomRel (string->symbol atomRel)))
       ;            (equal? (format "~v" atomRel) (format "~v" tupElem)))
       ;          (forge:Run-atoms context)))
       ;(cond [(equal? 1 (length filterResult)) (node/expr/atom info 1 (first filterResult))]
       ;      [else (error (format "tup2Expr: ~a had <>1 result in atom rels: ~a" tupElem filterResult))]))
       (when (list? tupElem) (error (format "tupElem ~a in tuple ~a is a list" tupElem tuple)))
       (node/expr/atom info 1 tupElem))
     tuple))  
  (node/expr/op/-> info (length tupRelationList) tupRelationList))

; Helper used to flip the currentTupleIfAtomic in the transpose case 
(define (transposeTup tuple)
  (cond 
    [(equal? (length tuple) 2) (list (second tuple) (first tuple))]
    [else (error (format "transpose tuple for tup ~a isn't arity 2. It has arity ~a" tuple (length tuple) ))]))

; This helper checks the value of currTupIfAtomic and throws an error if it is empty. 
(define (mustHaveTupleContext tup expr)
  (cond
    [(not(list? tup)) (error (format "currTupIfAtomic is not a list in ~a" expr))]
    [(equal? (length tup) 0)  (error (format "currTupIfAtomic has length 0 in ~a" expr))]
    [(list? (first tup)) (error (format "currTupIfAtomic ~a is not a tuple in ~a" tup expr))]))

; Function isGroundProduct used to test whether a given expression is ground. 
(define (isGroundProduct expr)
  (cond
    [(not (node/expr? expr)) (error (format "expression ~a is not an expression." expr))]
    ; Check if the expression is UNARY and if SUM or SING type. If so, call the function recursively. 
    [(and (checkIfUnary expr) (or (node/expr/op/sing? expr) (node/int/op/sum? expr)))
     (define args (node/expr/op-children expr))
     (isGroundProduct (first args))]
    ; If the expression is a quantifier variable, return true 
    ;[(node/expr/quantifier-var? expr) (error (format "isGroundProduct called on variable ~a" expr))]
    [(node/expr/quantifier-var? expr) #t]
    ; If the expression is binary and of type PRODUCT, call function recurisvely on LHS and RHS of expr
    [(node/expr/op/->? expr)
     (define args (node/expr/op-children expr))
     (andmap isGroundProduct args)]
    ; If the expression is a constant and a number, return true 
    [(and (node/expr/constant? expr) (number? (node/expr/constant expr))) #t]
    ; atoms are also a base case
    [(node/expr/atom? expr) #t]
    ; If none of the above cases are true, then return false
    [else #f]
    ))

; Function that takes in a given expression and returns whether that expression is unary
; This function tests whether the given expression is transitive closure, reflexive transitive
; closure, transpose, or sing. 
(define (checkIfUnary expr)
  (or (node/expr/op/^? expr)
      (node/expr/op/*? expr)
      (node/expr/op/~? expr)
      (node/expr/op/sing? expr)
      ))

; Function that takes in a given expression and returns whether that expression is binary.
; This function tests whether the given expression is a set union, set subtraction, set
; intersection, product, or join. 
(define (checkIfBinary expr)
  (or (node/expr/op/+? expr)
      (node/expr/op/-? expr)
      (node/expr/op/&? expr)
      (node/expr/op/->? expr)
      (node/expr/op/join? expr)
      ))

; tuples are just Racket lists. remember that start is ZERO INDEXED
(define (projectTupleRange tup start len)
  (take (list-tail tup start) len))

; Helper to get the right column of a relation. This should have the join on the LHS
; (univ.(univ.node)) for node with arity 3
(define (getColumnRight node)
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond [(equal? 0 arity) (error (format "getColumnRight arity <1: ~a" node))]
        [(equal? 1 arity) node]
        [else (getColumnRight (node/expr/op/join info (- arity 1) (list univ node)))]))

; Helper to get the left column of a relation. This should have the join on the RHS
; (node.univ).univ for node with arity 3
(define (getColumnLeft node)
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond [(equal? 0 arity) (error (format "getColumnRight arity <1: ~a" node))]
        [(equal? 1 arity) node]
        [else (getColumnRight (node/expr/op/join info (- arity 1) (list node univ)))]))


(define (createNewQuantifier decls quantvars form runContext info quantifier formula)
  (unless (equal? (length decls) 1)
    (error (format "createNewQuantifier: ~a" decls)))
  
  (define var (car (car decls)))
  (define domain (cdr (car decls)))
  (let ([quantvars (cons var quantvars)])    
    ; gives us list of all possible bindings to this variable
    (define lifted-bounds (lift-bounds-expr domain quantvars runContext))
    ; produce a list of subformulas each substituted with a possible binding for the variable
    (define subformulas (map
                         (lambda (tup)
                           (define result (substitute-formula form quantvars var (tup2Expr tup runContext info)))
                           (printf "debug for createNewQuantifier ~a: ~a~n" tup result)
                           result)
                         lifted-bounds))
    
    (cond [(equal? quantifier 'some) (node/formula/op/|| info subformulas)]
          [(equal? quantifier 'all) (node/formula/op/&& info subformulas)]
          [else (error (format "desugaring unsupported: ~a" formula))])))
