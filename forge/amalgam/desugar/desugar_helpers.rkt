#lang forge/core
(require debug/repl)
(require "../lift-bounds/lift-bounds.rkt")
(require "../substitutor/substitutor.rkt")
(require (prefix-in @ racket))
(require (prefix-in @ (only-in racket ->)))

(provide tup2Expr transposeTup mustHaveTupleContext isGroundProduct
         createNewQuantifier projectTupleRange getColumnRight 
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
     (cons (node/formula/op/in info (list (tup2Expr (list (first currTupIfAtomic))
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
  (transitiveClosureAnd filteredExtendResult expr info runContext runningArgs)
  (@-> (listof list?) node/expr? nodeinfo? forge:Run? list? node/formula/op/&&?)
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
;        right - the rightmost item in the currentResultPath
;        expr - the current expression we are looking at 
;        info - info of the original node 
;        runContent - current runContext
; 
; output: an in which looks like (in left->right edges)
; called by transitiveClosureAnd
(define/contract (transitiveClosureIn left right expr info runContext)
  (@-> list? list? node/expr? nodeinfo? forge:Run? node/formula/op/in?)
  (node/formula/op/in info
                      (list (tup2Expr (append left right) runContext info)
                            expr)))

; used by desugar. similar logic to buildClosureOfTupleSet
; input:
;     edges: upper-bound on possible tuples
;     prefix: the beginning point for all paths
; output: constructs all possible paths from <prefix> using <edges>
; to build set of paths starting with t0 using upperbnd UBA: (extendPossiblePaths UBA (list t0))

(define/contract (extendPossiblePaths edges prefix)
  (@-> (listof list?) list? (listof list?))
  (define newSimplePaths
    (filter-map
     (lambda (e)
       (define-values (e0 e1) (values (first e) (second e)))
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


; input: leftT - left hand side of joined tuple
;        rightT - right hand side of joined tuple
; 
; output: result of the join of leftT and rightT
; Helper to join Tuples together
; tuple, tuple -> tuple
(define/contract (joinTupleDesugar leftT rightT)
  (@-> (listof symbol?) (listof symbol?) (or/c boolean? (listof symbol?)))
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

(printf "second calling projectTupleRange tup: ~a, start: ~a arity: ~a"
          currTupIfAtomic
          0
         (node/expr-arity left))
  (define
    leftTupleContext
    ; (define/contract (projectTupleRange tup start len)
    ;  (@-> list? number? number? list?)
    ;  (take (list-tail tup start) len))
    (projectTupleRange currTupIfAtomic 0 (node/expr-arity left)))

  (printf "second calling projectTupleRange tup: ~a, start: ~a arity: ~a"
          currTupIfAtomic
          (- (node/expr-arity left) 1)
          (node/expr-arity right))
  (define
    rightTupleContext
    (projectTupleRange currTupIfAtomic
                       (- (node/expr-arity left) 1) (node/expr-arity right)))
  
  (list
     (node/formula/op/in info (list
                               (tup2Expr leftTupleContext runContext info)
                               left))
     (node/formula/op/in info (list
                               (tup2Expr rightTupleContext runContext info)
                               right))))


; input: tuple - the tuple that we want to convert into an expression 
;        context - the run context of the program 
;        info - info of original tuple 
; 
; output: the tuple as an expression, re-written as a node/expr/atom
; NOTE: doesn't handle ints
(define/contract (tup2Expr tuple context info)
  (@-> (or/c (listof symbol?) symbol?) forge:Run? nodeinfo? node/expr?)
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
     (node/expr/op/-> info (length tupRelationList) tupRelationList)]))

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
  (take (list-tail tup start) len))

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
        [else (getColumnRight (node/expr/op/join info (- arity 1)
                                                 (list univ node)))]))


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
(define/contract
  (createNewQuantifier decl quantVars subForm runContext info quantifier formula)
  (@-> pair? list? node/formula? forge:Run? nodeinfo?
       (or/c 'some 'all) node/formula?
       (or/c node/formula/op||? node/formula/op/&&? exn:fail?))
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
          [(equal? quantifier 'all) (node/formula/op/&& info subFormulas)])))

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