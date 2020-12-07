#lang forge/core
(require debug/repl)
(require "../lift-bounds/lift-bounds.rkt")
(require "../substitutor/substitutor.rkt")
(require (prefix-in @ racket))

(provide tup2Expr transposeTup mustHaveTupleContext isGroundProduct
         createNewQuantifier projectTupleRange getColumnRight getColumnLeft
         transitiveClosureHelper productHelper joinHelper joinTupleDesugar
         extendPossiblePaths transitiveClosureAnd )

; input: filteredExtendResult - the list of edges from desugar
;        expr - the current expression we are looking at 
;        info - info of the original node 
;        runContent - current runContext
;        runningArgs - starts as an empty list, eventually the arguments for AND
;
; output: recursively call transitiveClosureIn on first two items in list
;         and put in a big AND called by desugar
(define
  (transitiveClosureAnd filteredExtendResult expr info runContext runningArgs)
  (cond
    [(or
      (empty? filteredExtendResult)
      (equal? (length filteredExtendResult) 1))
     (node/formula/op/&& info runningArgs)]
    [else
     (transitiveClosureAnd
      (rest filteredExtendResult) expr info runContext
      (cons (transitiveClosureIn (first filteredExtendResult)
                                 (second filteredExtendResult)
                                 expr
                                 info
                                 runContext) runningArgs))]))


; input: left - the leftmost item in the currentResultPath
;        right - the rightmost item in the crurentResultPath
;        expr - the current expression we are looking at 
;        info - info of the original node 
;        runContent - current runContext
; 
; output: an in which looks like (in left->right edges)
; called by transitiveClosureAnd
(define (transitiveClosureIn left right expr info runContext)
  ;(debug-repl)
  (node/formula/op/in info
                      (list (tup2Expr (list left right) runContext info)
                            expr)))

; used by desugar. similar logic to buildClosureOfTupleSet
; given upper-bound on possible tuples, constructs all possible
; paths from <prefix> using <edges>
; TODO: account for <end>
; to build set of paths starting with t0 using upperbnd UBA: (extendPossiblePaths UBA (list t0))

; to produce desugared fmla:
;   '((1 2 3) (1 3 4) (1 2 5) (1 3 5))
; first filter to desired end point (e.g., 5)
;   '((1 2 5) (1 3 5))
; OR:
;   can follow: 1 to 2 to 5
;   can follow: 1 to 3 to 5
; OR:
;   AND: edge(1,2) edge(2,5)
;   AND: edge(1,3) edge(3,5) 

(define (extendPossiblePaths edges prefix)    
  (define newSimplePaths
    (filter-map
     (lambda (e)
       (define-values (e0 e1) (values (first e) (second e)))
       ; TODO: SO INEFFICIENT :-( but just write it
       ;  might be better to build paths in reverse from t1 than forward from t0
       ;  or use something other than lists for everything here
       (cond [(member e1 prefix) #f] ; only build simple paths
             [(equal? (last prefix) e0) (append prefix (list e1))]
             [else #f]))
     edges))
  
  (cond [(empty? newSimplePaths)
         empty]
        [else
         ; keep the new simple paths, but also try to extend them again
         (append newSimplePaths
                 (apply append (map (lambda (newPrefix)
                                      (extendPossiblePaths edges newPrefix))
                                    newSimplePaths)))]))


; input: leftTS - left hand side of joined tuple
;        rightTS - right hand side of joined tuple
; 
; output: result of the join of leftTS and rightTS
; Helper to join Tuples together
; list<tuple>, list<tuple> -> list<tuple>
(define (joinTupleDesugar leftTS rightTS)
  (cond
    [(equal? (last leftTS) (first rightTS))
     (append (take leftTS (- (length leftTS) 1)) (rest rightTS))]
    [else #f]))

; input: origExpr - the first argument of transitive closure 
;        listOfJoins - list containing all of the previous joins
;        totalNumOfDots - The total number of dots that we want to see in the
;        last join 
;        info - info of the original node 
; 
; output: list containing all of the joins of a transitive closure 
(define
  (transitiveClosureHelper
   origExpr listOfJoins totalNumOfDots currNumOfDots info)
  (cond
    [(equal? currNumOfDots totalNumOfDots) listOfJoins]
    [(equal? currNumOfDots 0)
     (transitiveClosureHelper
      origExpr (append listOfJoins (list origExpr))
      totalNumOfDots (+ currNumOfDots 1) info)]
    [else
     (define nextJoin
       (node/expr/op/join info (node/expr-arity origExpr)
                          (list (last listOfJoins) origExpr)))
     (transitiveClosureHelper
      origExpr (append listOfJoins (list nextJoin))
      totalNumOfDots (+ currNumOfDots 1) info)]))

; input: right - list of arguments
;        currTupIfAtomic - implicit LHS of expression
;        info - info of original expression
;        left - a list of arguments with previous results of the product
; 
; output: list containing two in nodes
(define (productHelper left right currTupIfAtomic info runContext)
  ;(debug-repl)
  (define LHS (last left))
  (define RHS right)
  (define
    leftTupleContext
    (projectTupleRange currTupIfAtomic 0 (node/expr-arity LHS)))
  (define
    rightTupleContext
    (projectTupleRange currTupIfAtomic
                       (- (node/expr-arity LHS) 1) (node/expr-arity RHS)))
  (define formulas
    (list
     (node/formula/op/in info (list
                               (tup2Expr leftTupleContext runContext info) LHS))
     (node/formula/op/in info (list
                               (tup2Expr rightTupleContext runContext info) RHS))))
  formulas)

; input:
;      expr: overall expression of the entire join we are looking at
;      left: left hand side of our join
;      right: right hand side of our join
;      info: info of original Node
; output: re-writing join as a 'some existential formula. We return a
;      quantified formula of all of the joins 
(define (joinHelper expr left right info)
  (define rightColLHS (getColumnRight left))
  (define leftColRHS (getColumnLeft right))
  (define listOfColumns (list leftColRHS rightColLHS))

  (define intersectColumns (node/expr/op/& info
                                           (node/expr-arity expr) listOfColumns))
  (define quantifiedVarJoin (node/expr/quantifier-var info 1 (gensym "join")))
  (define newDecls (list (cons quantifiedVarJoin intersectColumns)))
 
  (define LHSRange (projectTupleRange quantifiedVarJoin 0
                                      (- (node/expr-arity left) 1)))
  (define RHSRange (projectTupleRange quantifiedVarJoin
                                      (node/expr-arity left)
                                      (node/expr-arity right)))
  
  (define LHSProduct (node/expr/op/-> info
                                      (node/expr-arity intersectColumns)
                                      (list LHSRange quantifiedVarJoin)))
  (define RHSProduct (node/expr/op/-> info
                                      (node/expr-arity intersectColumns)
                                      (list quantifiedVarJoin RHSRange)))
  (define LHSIn (node/formula/op/in info (list LHSProduct left)))
  (define RHSIn (node/formula/op/in info (list RHSProduct right)))

  (define joinAnd (node/formula/op/&& info (list LHSIn RHSIn)))
  (node/formula/quantified info 'some newDecls joinAnd))

; input: tuple - the tuple that we want to convert into an expression 
;        context - the run context of the program 
;        info - info of original tuple 
; 
; output: the tuple as an expression, re-written as a node/expr/atom 
(define (tup2Expr tuple context info)
  (when (equal? (length tuple) 0)
    (error (format "tupElem ~a is an empty list" tuple)))
  (when (not (list? tuple))
    (error (format "tupElem ~a is not a list" tuple)))
  (define tupRelationList
    ; replace every element of the tuple (atoms) with the corresponding atom relation
    (map
     (lambda (tupElem)
       (when (list? tupElem)
         (error (format "tupElem ~a in tuple ~a is a list" tupElem tuple)))
       (node/expr/atom info 1 tupElem))
     tuple))  
  (node/expr/op/-> info (length tupRelationList) tupRelationList))

; input: tuple - the tuple that we want to flip 
; 
; output: flipped (transposed) tuple  
(define (transposeTup tuple)
  (cond 
    [(equal? (length tuple) 2) (list (second tuple) (first tuple))]
    [else
     (error
      (format "transpose tuple for tup ~a isn't arity 2. It has arity ~a" tuple
              (length tuple) ))]))

; input:
;      tup: The currentTupleIfAtomic that we are evaluating 
;      expr: The expression that contains the currentTupleIfAtomic
;
; output: throws an error if the currentTupleIfAtomic doesn't have a context 
(define (mustHaveTupleContext tup expr)
  (cond
    [(not(list? tup))
     (error (format "currTupIfAtomic is not a list in ~a" expr))]
    [(equal? (length tup) 0)
     (error (format "currTupIfAtomic has length 0 in ~a" expr))]
    [(list? (first tup))
     (error (format "currTupIfAtomic ~a is not a tuple in ~a" tup expr))]))

; input:
;      expr: A given expression that has the possibility of being ground 
;
; output: returns true if the expr is ground, false if it isn't, and an
; error if something went wrong. 
(define (isGroundProduct expr)
  (cond
    [(not (or (node/expr? expr) (node/int/constant? expr)))
     (error (format "expression ~a is not an expression or int constant." expr))]
    [(and (checkIfUnary expr) (or (node/expr/op/sing? expr)
                                  (node/int/op/sum? expr)))
     (define args (node/expr/op-children expr))
     (isGroundProduct (first args))]
    [(node/expr/quantifier-var? expr)
     (error (format "isGroundProduct called on variable ~a" expr))]
    [(node/expr/op/->? expr)
     (define args (node/expr/op-children expr))
     (andmap isGroundProduct args)]
    [(node/int/constant? expr) #t]
    [(node/expr/atom? expr) #t]
    [else #f]
    ))

; input:
;      tup: The current tuple that we are getting the range of
;      start: starting index
;      len: end index 
;
; output: Returns the range of a given tuple 
(define (projectTupleRange tup start len)
  (take (list-tail tup start) len))

; input:
;      node: The node that we want to get the right column of 
;
; output: Returns the right column of a given node.
; For node with arity 3, it returns (univ.(univ.node))
(define (getColumnRight node)
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond [(equal? 0 arity) (error (format "getColumnRight arity <1: ~a" node))]
        [(equal? 1 arity) node]
        [else (getColumnRight (node/expr/op/join info (- arity 1)
                                                 (list univ node)))]))

; input:
;      node: The node that we want to get the left column of 
;
; output: Returns the right column of a given node. For node with arity 3,
; it returns (node.univ).univ
(define (getColumnLeft node)
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond [(equal? 0 arity) (error (format "getColumnRight arity <1: ~a" node))]
        [(equal? 1 arity) node]
        [else (getColumnRight (node/expr/op/join info (- arity 1)
                                                 (list node univ)))]))

; input:
;      decl: The original decl for the quantifier that we are trying to re-write
;      quantVars: the quantifiable variables of the run that we're calling this
;        helper from
;      subForm: the original subformula from the quantifier that we are re-writing
;      runContext: the context for the program
;      info: the original info of the node
;      quantifier: the quantifier from the formula that we are trying to re-write
;      formula: the quantifier formula 
;
; output: Returns a big AND or OR of the subformulas 
(define
  (createNewQuantifier decl quantVars subForm runContext info quantifier formula)
  (unless (not (and (null? (car decl)) (null? (cdr decl))))
    (error (format "createNewQuantifier: decl ~a is not a tuple" decl)))
  (define var (car decl)) 
  (define domain (cdr decl)) 
  (let ([quantVars (cons var quantVars)])  
    ; gives us list of all possible bindings to this variable
    (define liftedBounds (liftBoundsExpr domain quantVars runContext))
    ; produce a list of subFormulas each substituted with a possible binding
    ; for the variable
    (define subFormulas
      (map (lambda (tup)
             (substituteFormula subForm quantVars var
                                (tup2Expr tup runContext info)))
           liftedBounds))
    
    (cond [(equal? quantifier 'some) (node/formula/op/|| info subFormulas)]
          [(equal? quantifier 'all) (node/formula/op/&& info subFormulas)]
          [else (error (format "desugaring unsupported: ~a" formula))])))

; input:
;      expr: A given expression 
;
; output: returns true if the expression is unary, false if not. 
(define (checkIfUnary expr)
  (or (node/expr/op/^? expr)
      (node/expr/op/*? expr)
      (node/expr/op/~? expr)
      (node/expr/op/sing? expr)))

; input:
;      expr: A given expression 
;
; output: returns true if the expression is binary, false if not. 
(define (checkIfBinary expr)
  (or (node/expr/op/+? expr)
      (node/expr/op/-? expr)
      (node/expr/op/&? expr)
      (node/expr/op/->? expr)
      (node/expr/op/join? expr)))