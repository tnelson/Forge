#lang forge/core

(provide tup2Expr transposeTup mustHaveTupleContext isGroundProduct checkIfUnary checkIfBinary create-bitwidth-list)

; return list of lists inclusive of start and end
(define (create-bitwidth-list start end)
  (cond
    [(equal? start (+ end 1)) '()]
    [else
      (cons (list start) (create-bitwidth-list (+ 1 start) end))]))

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
         [(equal? (length tuple) 2) (list (second tuple) (first tuple))]
         [else (error "transpose tuple for tup ~a isn't arity 2. It has arity ~a" tuple (length tuple))]))

; This helper checks the value of currTupIfAtomic and throws an error if it is empty. 
(define (mustHaveTupleContext tup)
  (cond
    [(equal? (length tup) 0) (error "currTupIfAtomic is empty and it shouldn't be")]))

; Function isGroundProduct used to test whether a given expression is ground. 
(define (isGroundProduct expr)
  (cond
    [(not (node/expr? expr)) (error "expression ~a is not an expression." expr)]
    ; Check if the expression is UNARY and if SUM or SING type. If so, call the function recursively. 
    [(and (checkIfUnary expr) (or (node/expr/op/sing? expr) (node/int/op/sum? expr)))
     (define args (node/expr/op-children expr))
     (isGroundProduct (first args))]
    ; If the expression is a quantifier variable, return true 
    [(node/expr/quantifier-var? expr) (error (format "isGroundProduct called on variable ~a" expr))]
    ; If the expression is binary and of type PRODUCT, call function recurisvely on LHS and RHS of expr 
    [(and (checkIfBinary expr) (node/expr/op/->? expr))
     (define args (node/expr/op-children expr))
     (and (isGroundProduct (first args)) (isGroundProduct (second args)))]
    ; If the expression is a constant and a number, return true 
    [(and (node/expr/constant? expr) (number? (node/expr/constant expr))) #t]
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

; Zip method used to get the join between bounds in lift-bounds.rkt.
; This method works as follows: it takes in two lists, l1 and l2,
; where l1 = '(1 2) l2 = '(3 4) and returns '((1 3) (2 4))
(define zip (lambda (l1 l2) (map list l1 l2)))
