
#lang forge/core

(require "../lift-bounds/lift-bounds.rkt")
(require "../lift-bounds/lift-bounds_helpers.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require debug/repl)
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(define node-bound (list (list 'Node0) (list 'Node1) (list 'Node2) (list 'Node3) (list 'Node4) (list 'Node5) (list 'Node6)))
(define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define var-expr-constant (node/expr/constant empty-nodeinfo 1 'Int))
(define var-int-const-x (node/int/constant empty-nodeinfo 1))
(define var-int-const-y (node/int/constant empty-nodeinfo 2))
(define int-bounds (list (list -8) (list -7) (list -6) (list -5) (list -4) (list -3) (list -2) (list -1) (list 0) (list 1) (list 2) (list 3) (list 4) (list 5) (list 6) (list 7)))

; Checking atom case base case 
(@test-case
 "TEST lift-bounds-expr on atom base case"
(define sampleAtom (node/expr/atom empty-nodeinfo 1 Node))
(@check-equal?
 (to-string (lift-bounds-expr sampleAtom '() udt))
 (to-string (list (list (node/expr/atom-name sampleAtom))))))

; Checking relation name case base case 
(@test-case
 "TEST lift-bounds-expr on relation base case"
(@check-equal?
 (to-string (lift-bounds-expr Node '() udt))
 (to-string node-bound)))

; Checking Int constant case base case
(@test-case
 "TEST lift-bounds-expr on int constant base case"
(@check-equal?
 (to-string (lift-bounds-expr var-expr-constant '() udt))
 (to-string int-bounds)))

; Checking other esxpression constants base case
; UNIV
(@test-case
 "TEST lift-bounds-expr on constant univ base case"
(define expressionConstantUNIV (node/expr/constant empty-nodeinfo 1 'univ))
(@check-equal?
 (to-string (lift-bounds-expr expressionConstantUNIV '() udt))
 (to-string (map (lambda (x) (list x x)) (forge:Run-atoms udt)))))

; IDEN
(@test-case
 "TEST lift-bounds-expr on constant iden base case"
(define expressionConstantIDEN (node/expr/constant empty-nodeinfo 1 'iden))
(@check-equal?
 (to-string (lift-bounds-expr expressionConstantIDEN '() udt))
 (to-string (map (lambda (x) (list x x)) (forge:Run-atoms udt)))))

; Checking Quantified variable
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(@check-exn
 exn:fail?
 (lambda () 
   (lift-bounds-expr f-some-reaches-all '() udt)))

; Checking Set Comprehension constant case
(@test-case
 "TEST lift-bounds-expr on set comprehension case"
(define qvx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define f-set-comprehension (node/expr/comprehension empty-nodeinfo 1
                                  (list (cons qvx Node))
                                  (in qvx Node)))
(define uppers-set-comprehension (list (lift-bounds-expr Node '(qvx) udt)))

(@check-equal?
 (to-string (lift-bounds-expr f-set-comprehension '() udt))
 (to-string (map (lambda (ub) (apply append ub)) (apply cartesian-product uppers-set-comprehension)))))


; Checking Set union case
(@test-case
 "TEST lift-bounds-expr on set union case"
(define uppers-union (list node-bound (map (lambda (x) (list x x)) (forge:Run-atoms udt))))
(@check-equal?
 (to-string (lift-bounds-expr (+ Node univ) '() udt))
 (to-string (remove-duplicates (apply append uppers-union)))))

; Checking Set minus case
(@test-case
 "TEST lift-bounds-expr on set minus case"
(@check-equal?
 (to-string (lift-bounds-expr (- Node univ) '() udt))
 (to-string node-bound)))

; Checking Set intersection case
(@test-case
 "TEST lift-bounds-expr on set intersection case"
(@check-equal?
 (to-string (lift-bounds-expr (& Node (- Node (+ Node univ))) '() udt))
 (to-string node-bound)))

; Checking Set Product case
(@test-case
 "TEST lift-bounds-expr on set product case"
(define LHSProduct-bounds (lift-bounds-expr Node '() udt))
(define RHSProduct-bounds (lift-bounds-expr (-> Node univ) '() udt))
(define list-product-bounds (list LHSProduct-bounds RHSProduct-bounds))
(define product-map (map (lambda (ub) (apply append ub)) (apply cartesian-product list-product-bounds)))

(@check-equal?
 (to-string (lift-bounds-expr (-> Node (-> Node univ)) '() udt))
 (to-string product-map)))

; Checking Set Join case
; Error case arity < 1
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-expr (join Node Node) '() udt)))

; testing normal join case with two arguments  
(@test-case
 "TEST lift-bounds-expr on join case"
(define join-LHS (lift-bounds-expr edges '() udt))
(define join-RHS (lift-bounds-expr iden '() udt))
(define list-join (list join-LHS join-RHS))
(define newTuples (joinTuple (first list-join) (second list-join)))

(@check-equal?
 (to-string (lift-bounds-expr (join edges iden) '() udt))
 (to-string newTuples)))


; Checking join with more than two arguments 
(@test-case
 "TEST lift-bounds-expr on more complicated join case"
(define join-LHS-more (lift-bounds-expr edges '() udt))
(define join-RHS-more (lift-bounds-expr iden '() udt))
(define list-join-more (list join-LHS-more join-RHS-more join-RHS-more))
(define newTuples-more (joinTuple (first list-join-more) (second list-join-more)))
(define foldNewTuples (foldl (lambda (curr acc) (joinTuple acc curr)) newTuples-more (rest (rest list-join-more))))

(@check-equal?
 (to-string (lift-bounds-expr (join edges iden iden) '() udt))
 (to-string foldNewTuples)))

; Checking Set transitive closure case
(@test-case
 "TEST lift-bounds-expr on set transitive closure case"
(define transitive-closure-bounds (lift-bounds-expr edges '() udt))
(@check-equal?
 (to-string (lift-bounds-expr (^ edges) '() udt))
 (to-string (buildClosureOfTupleSet transitive-closure-bounds))))

; Checking Set reflexive transitive closure case
(@test-case
 "TEST lift-bounds-expr on set reflexive transitive closure case"
(define reflexive-transitive-closure-bounds (lift-bounds-expr edges '() udt))
(define closureOfTupleSets (buildClosureOfTupleSet reflexive-transitive-closure-bounds))

(@check-equal?
 (to-string (lift-bounds-expr (* edges) '() udt))
 (to-string (remove-duplicates (append closureOfTupleSets (map (lambda (x) (list x x)) (forge:Run-atoms udt)))))))


; Checking Set transpose case
(@test-case
 "TEST lift-bounds-expr on set transpose case"
(define transpose-bounds (list (lift-bounds-expr edges '() udt)))
 (@check-equal?
 (to-string (lift-bounds-expr (~ edges) '() udt))
 (to-string (map (lambda (x) (transposeTup x)) (first transpose-bounds)))))

; Checking Set singleton case
(@test-case
 "TEST lift-bounds-expr on set singleton case"
(@check-equal?
 (to-string (lift-bounds-expr (sing var-int-const-x) '() udt))
 (to-string int-bounds)))

; Checking const int case
(@test-case
 "TEST lift-bounds-expr on const int case"
(@check-equal?
 (to-string (lift-bounds-int var-int-const-x '() udt))
 (to-string int-bounds)))

; Checking int with operator (should error)
(define f-int-less (< var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-int f-int-less '() udt)))

(define f-int-greater (> var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-int f-int-greater '() udt)))

; Checking sum "quantifier" case
(@test-case
 "TEST lift-bounds-expr on sum quantifier case"
(define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define f-sum (node/int/sum-quant empty-nodeinfo
                                  (list (cons x Node))
                                  (node/int/op/card empty-nodeinfo (list (join edges x)))))
(@check-equal?
 (to-string (lift-bounds-int f-sum '() udt))
 (to-string int-bounds)))


;Checking cardinality case
(@test-case
 "TEST lift-bounds-expr on cardinality case"
(define f-cardinality (node/int/op/card empty-nodeinfo (list Node)))
(@check-equal?
 (to-string (lift-bounds-int f-cardinality '() udt))
 (to-string int-bounds)))
