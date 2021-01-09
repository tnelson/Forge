#lang forge/core
(require debug/repl)
(require "../lift-bounds/lift-bounds.rkt")
(require "../substitutor/substitutor.rkt")
(require (prefix-in @ racket))
(require (prefix-in @ (only-in racket ->)))

(provide tup2Expr transposeTup mustHaveTupleContext isGroundProduct
         createNewQuant projectTupleRange getColumnRight 
         productHelper joinTupleDesugar extendPossiblePaths transitiveClosureAnd)

; input: currTupIfAtomic - implicit LHS
;        quantVars - 
;        decls - the declarations in the current set comprehension
;        info - info of the original node
;
; output: recursively create an in formula of the currTupIfAtomic and
;         the corresopnding decl at the same index
(define/contract (setComprehensionAndHelper currTupIfAtomic decls info runContext)
  (@-> (listof symbol?) (listof pair?) nodeinfo? forge:Run?
       (listof node/formula/op/in?))
  (cond
    [(empty? decls) '()]
    [else
     (cons (in/info info (list (tup2Expr (list (first currTupIfAtomic))
                                                    runContext info)
                                          (cdr (first decls))))
           (setComprehensionAndHelper (rest currTupIfAtomic)
                                      (rest decls) info runContext))]))

; input: form - the current formula being desugared
;        currTupIfAtomic - implicit LHS
;        quantVars - 
;        decls - the declarations in the current set comprehension
;
; output: recursively create a quantifier-var expression containing each
;         declaration and the corresponding quantVar
(define/contract (setComprehensionSubHelper form currTupIfAtomic quantVars decls
                                   runContext info)
  (@-> node/formula? (listof symbol?) list? (listof pair?) forge:Run?
       nodeinfo? node/formula?)
  (cond
    [(empty? decls) form]
    [else
     (define formulaSoFar (substituteFormula form quantVars (car (first decls))
                              (tup2Expr (list (first currTupIfAtomic)) runContext info)))
     (setComprehensionSubHelper formulaSoFar (rest currTupIfAtomic) quantVars
                                      (rest decls) runContext info)]))

; input: filteredExtendResult - the list of edges from desugar
;        expr - the current expression we are looking at 
;        info - info of the original node 
;        runContent - current runContext
;        runningArgs - starts as an empty list, eventually the arguments for AND
;
; output: recursively call transitiveClosureIn on first two items in list
;         and put in a big AND called by desugar
(define/contract
  (transitiveClosureAnd path expr info runContext runningArgs)
  (@-> list? node/expr? nodeinfo? forge:Run? list? node/formula/op/&&?)
  (cond
    [(or
      (empty? path)
      (equal? (length path) 1))
     (&&/info info runningArgs)]
    [else
     (transitiveClosureAnd
      (rest path) expr info runContext
      (cons (transitiveClosureIn (first path)
                                 (second path)
                                 expr
                                 info
                                 runContext) runningArgs))]))


; input: left - the leftmost item in the currentResultPath
;        right - the rightmost item in the currentResultPath
;        expr - the current expression we are looking at 
;        info - info of the original node 
;        runContent - current runContext
; 
; output: an in which looks like (in left->right edges)
; called by transitiveClosureAnd
(define/contract (transitiveClosureIn left right expr info runContext)
  (@-> symbol? symbol? node/expr? nodeinfo? forge:Run? node/formula/op/in?)
  (in/info info (list (tup2Expr (list left right) runContext info) expr)))

; used by desugar. similar logic to buildClosureOfTupleSet
; input:
;     edges: upper-bound on possible tuples
;     prefix: the beginning point for all paths
; output: constructs all possible paths from <prefix> using <edges>
; to build set of paths starting with t0 using upperbnd UBA: (extendPossiblePaths UBA (list t0))

(define/contract (extendPossiblePaths edges prefix)
  (@-> (listof list?) list? (listof list?))
  (define newPaths
    (filter-map
     (lambda (e)
       (define-values (e0 e1) (values (first e) (second e)))
       ; Don't build only simple paths at this point
       (cond ;[(member e1 prefix) #f] ; only build simple paths
             [(equal? (last prefix) e0) (append prefix (list e1))]
             [else #f]))
     edges))

  ; weed out paths that cycle back in last element
  (define newSimplePaths
    (filter (lambda (p) (not (member (last p) (drop-right p 1)))) newPaths))
  
  (cond [(empty? newPaths)
         empty]
        [else
         ; keep ALL the new paths, but also try to extend simple ones again
         (append newPaths
                 (apply append (map (lambda (newPrefix)
                                      (extendPossiblePaths edges newPrefix))
                                    newSimplePaths)))]))


; input: leftT - left hand side of joined tuple
;        rightT - right hand side of joined tuple
; 
; output: result of the join of leftT and rightT
; Helper to join Tuples together
; tuple, tuple -> tuple
(define/contract (joinTupleDesugar leftT rightT)
  (@-> (listof (or/c symbol? number?)) (listof (or/c symbol? number?))
       (or/c boolean? (listof (or/c symbol? number?))))
  (cond
    [(equal? (last leftT) (first rightT))
     (append (take leftT (- (length leftT) 1)) (rest rightT))]
    [else #f]))

; input: right - second argument
;        currTupIfAtomic - implicit LHS of expression
;        info - info of original expression
;        left - first argument
; 
; output: list containing two in nodes
(define/contract (productHelper left right currTupIfAtomic info runContext)
  (@-> node/expr? node/expr? (listof symbol?) nodeinfo?
       forge:Run? (listof node/formula/op/in?))
  (define
    leftTupleContext
    (projectTupleRange currTupIfAtomic 0 (node/expr-arity left)))
  (define
    rightTupleContext
    (projectTupleRange currTupIfAtomic
                       (node/expr-arity left) (length currTupIfAtomic)))
  (list
     (in/info info (list (tup2Expr leftTupleContext runContext info) left))
     (in/info info (list (tup2Expr rightTupleContext runContext info) right))))


; input: tuple - the tuple that we want to convert into an expression 
;        context - the run context of the program 
;        info - info of original tuple 
; 
; output: the tuple as an expression, re-written as a node/expr/atom
(define/contract (tup2Expr tuple context info)
  (@-> (or/c (listof (or/c symbol? number?)) symbol? number?) forge:Run?
       nodeinfo? node/expr?)
  (cond
    [(symbol? tuple) (node/expr/atom info 1 tuple)]
    [(equal? (length tuple) 0)
     (error (format "tupElem ~a is an empty list" tuple))]
    [else
     (define tupRelationList
       ; replace every element of the tuple (atoms) with the corresponding atom
       ; relation
       (map
        (lambda (tupElem)
          (when (list? tupElem)
            (error (format "tupElem ~a in tuple ~a is a list" tupElem tuple)))
          (node/expr/atom info 1 tupElem))
        tuple))
     (cond
       [(equal? (length tupRelationList) 1) (first tupRelationList)]
       [else
        (node/expr/op/-> info (length tupRelationList) tupRelationList)])]))

; input: tuple - the tuple that we want to flip 
; 
; output: flipped (transposed) tuple  
(define/contract (transposeTup tuple)
  (@-> list? list?)
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
(define/contract (isGroundProduct expr)
  (@-> node? (or/c exn:fail? boolean?))
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
(define/contract (projectTupleRange tup start len)
  (@-> list? number? number? list?)
  (take (list-tail tup start) (- len start)))

; input:
;      node: The node that we want to get the right column of 
;
; output: Returns the right column of a given node.
; For node with arity 3, it returns (univ.(univ.node))
(define/contract (getColumnRight node)
  (@-> node? (or/c exn:fail? node?))
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond [(equal? 0 arity) (error (format "getColumnRight arity <1: ~a" node))]
        [(equal? 1 arity) node]
        [else (getColumnRight (join/info info (list univ node)))]))

; input:
;      node: The node that we want to get the left column of 
;
; output: Returns the right column of a given node. For node with arity 3,
; it returns (node.univ).univ
(define (getColumnLeft node)
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond [(equal? 0 arity) (error (format "getColumnLeft arity <1: ~a" node))]
        [(equal? 1 arity) node]
        [else (getColumnLeft (join/info info (list node univ)))]))

; If I want to get the first col, then desCol is 1 and colSoFar is 1 
(define/contract (getGivenColumn node desCol colSoFar initialArity)
  (@-> node? number? number? number? (or/c exn:fail? node))
  (define arity (node/expr-arity node))
  (define info (node-info node))
  (cond
    [(@> desCol initialArity) (error (format "getGivenColumn out of bounds col"))]
    [(equal? 0 arity) (error (format "getGivenColumn arity <1: ~a" node))]
    [(equal? 0 desCol) (getColumnLeft node)]
    [(and (equal? colSoFar desCol) (equal? arity 1)) node]
    [(and (equal? colSoFar desCol) (not (equal? arity 1))) (getColumnLeft node)]
    [else (getGivenColumn
           (join/info info (list univ node)) desCol (+ colSoFar 1) initialArity)]))

; input:
;      decls: This takes in the list of decls 
;      subForm: The formula that we are substituting in (or have substituted already
;      previously
;      quantifier: the quantifier from the formula that we are trying to re-write
;
; output: Returns a big AND or OR of the subformulas 
(define (createNewQuant decls quantVars subForm runContext info quantifier)
  (define var (car (first decls)))
  (define domain (cdr (first decls)))
  (let ([quantVars (cons var quantVars)])
    (define liftedBounds (liftBoundsExpr domain quantVars runContext))
    (define subFormulas
      (map (lambda (tup)
             (cond
               [(equal? quantifier 'some)
                (&&/info info
                         (list (in/info info (list tup domain))
                               (substituteFormula subForm quantVars var
                                (tup2Expr tup runContext info))))]
               [(equal? quantifier 'all)
                (=>/info info
                         (list (in/info info (list tup domain))
                         (substituteFormula subForm quantVars var
                                (tup2Expr tup runContext info))))])) liftedBounds))
    (cond
      [(and (equal? quantifier 'some) (equal? (length decls) 1))
       (||/info info subFormulas)]
      [(and (equal? quantifier 'all) (equal? (length decls) 1))
       (&&/info info subFormulas)]
      [(and (equal? quantifier 'some) (not (equal? (length decls) 1)))
        (createNewQuant (rest decls) quantVars (||/info info subFormulas) runContext info quantifier)]
      [(and (equal? quantifier 'all) (not (equal? (length decls) 1)))
       (createNewQuant (rest decls) quantVars (&&/info info subFormulas) runContext info quantifier)])))
  

; input:
;      expr: A given expression 
;
; output: returns true if the expression is unary, false if not. 
(define/contract (checkIfUnary expr)
  (@-> node? boolean?)
  (or (node/expr/op/^? expr)
      (node/expr/op/*? expr)
      (node/expr/op/~? expr)
      (node/expr/op/sing? expr)))