#lang forge/core
(require debug/repl)
(provide tup2Expr transposeTup mustHaveTupleContext isGroundProduct
         createNewQuantifier projectTupleRange getColumnRight getColumnLeft)
(require "../lift-bounds/lift-bounds.rkt")
(require "../substitutor/substitutor.rkt")
(require (prefix-in @ racket))

; input: right - list of arguments
;        currTupIfAtomic - implicit LHS of expression
;        info - info of original expression
;        left - a list of arguments with previous results of the product
; 
; output: list containing two in nodes
(define (product-helper left right currTupIfAtomic info runContext)
  (define LHS (last left))
  (define RHS (first right))
  (define leftTupleContext  (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
  (define rightTupleContext (projectTupleRange currTupIfAtomic (- (node/expr-arity LHS) 1) (node/expr-arity RHS)))
  (define formulas (list
                    (node/formula/op/in info (list (tup2Expr leftTupleContext runContext info) LHS))
                    (node/formula/op/in info (list (tup2Expr rightTupleContext runContext info) RHS))))
  formulas)

; input:
;      left: left hand side of our join
;      right: right hand side of our join
;      expr: overall expression of the entire join we are looking at
;      info: info of original Node
; output: quantified some representing join
; return a list of LHS and RHS to be combined into a big AND
(define (join-helper expr left right info)
  (define rightColLHS (getColumnRight left))
  (define leftColRHS (getColumnLeft right))
  (define listOfColumns (list leftColRHS rightColLHS))

  ; intersectColumns is a part of decls
  (define intersectColumns (node/expr/op/& info (node/expr-arity expr) listOfColumns))
  (define x (node/expr/quantifier-var info 1 (gensym "join")))
  (define new-decls (list (cons x intersectColumns)))
 
  (define LHSRange (projectTupleRange x 0 (- (node/expr-arity left) 1)))
  (define RHSRange (projectTupleRange x (node/expr-arity left) (node/expr-arity right)))
  
  (define LHSProduct (node/expr/op/-> info (node/expr-arity intersectColumns) (list LHSRange x)))
  (define RHSProduct (node/expr/op/-> info (node/expr-arity intersectColumns) (list x RHSRange)))
  (define LHSIn (node/formula/op/in info (list LHSProduct left)))
  (define RHSIn (node/formula/op/in info (list RHSProduct right)))

  (define join-and (node/formula/op/&& info (list LHSIn RHSIn)))
  (node/formula/quantified info 'some new-decls join-and))


; Helper to transform a given tuple from the lifted-upper bounds function to a relation, and then do the product of all relations
; to form an expression.
; <info> argument is optional; if not passed, will default to empty-nodeinfo
(define (tup2Expr tuple context info)
  (when (equal? (length tuple) 0) (error (format "tupElem ~a is an empty list" tuple)))
  (when (not (list? tuple)) (error (format "tupElem ~a is not a list" tuple)))
  (define tupRelationList
    ; replace every element of the tuple (atoms) with the corresponding atom relation
    (map
     (lambda (tupElem)
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
    ; we are not supporting SING or SUM
    [(and (checkIfUnary expr) (or (node/expr/op/sing? expr) (node/int/op/sum? expr)))
     (define args (node/expr/op-children expr))
     (isGroundProduct (first args))]
    ; If the expression is a quantifier variable, return true 
    ;[(node/expr/quantifier-var? expr) (error (format "isGroundProduct called on variable ~a" expr))]
    [(node/expr/quantifier-var? expr) #t]
    ; If the expression is of type PRODUCT, call function recurisvely on LHS and RHS of expr
    [(node/expr/op/->? expr)
     (define args (node/expr/op-children expr))
     (andmap isGroundProduct args)]
    ; If the expression is a constant and a number, return true 
    [(and (node/expr/constant? expr) (equal? 'Int (node/expr/constant-type expr))) #t]
    ; atoms are also a base case
    [(node/expr/atom? expr) #t]
    ; TODO: Should we check if expr is a relation?
    ; If none of the above cases are true, then return false
    [else #f]
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

(define (createNewQuantifier decl quantvars subForm runContext info quantifier formula)
  (unless (not (and (null? (car decl)) (null? (cdr decl))))
    (error (format "createNewQuantifier: decl ~a is not a tuple" decl)))
  (define var (car decl)) 
  (define domain (cdr decl)) 
  (let ([quantvars (cons var quantvars)])  
    ; gives us list of all possible bindings to this variable
    (define lifted-bounds (lift-bounds-expr domain quantvars runContext))
    ; produce a list of subformulas each substituted with a possible binding for the variable
    (define subformulas (map (lambda (tup)
                               (substitute-formula subForm quantvars var (tup2Expr tup runContext info)))
                             lifted-bounds))
    
    (cond [(equal? quantifier 'some) (node/formula/op/|| info subformulas)]
          [(equal? quantifier 'all) (node/formula/op/&& info subformulas)]
          [else (error (format "desugaring unsupported: ~a" formula))])))

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
