#lang forge/core

(require "lift-bounds.rkt")
(require "lift-bounds_helpers.rkt")
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
(printf "TEST 1 ~n~n")
(define sampleAtom (node/expr/atom empty-nodeinfo 1 Node))
(@check-equal?
 (to-string (lift-bounds-expr sampleAtom '() udt))
 (to-string (list (list (node/expr/atom-name sampleAtom)))))

; Checking relation name case base case 
(printf "TEST 2 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr Node '() udt))
 (to-string node-bound))

; Checking Int constant case base case
(printf "TEST 3 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr var-expr-constant '() udt))
 (to-string int-bounds))

; Checking other esxpression constants base case
; UNIV
(printf "TEST 4 ~n~n")
(define expressionConstantUNIV (node/expr/constant empty-nodeinfo 1 'univ))
(@check-equal?
 (to-string (lift-bounds-expr expressionConstantUNIV '() udt))
 (to-string (map (lambda (x) (list x x)) (forge:Run-atoms udt))))

; IDEN
(printf "TEST 5 ~n~n")
(define expressionConstantIDEN (node/expr/constant empty-nodeinfo 1 'iden))
(@check-equal?
 (to-string (lift-bounds-expr expressionConstantIDEN '() udt))
 (to-string (map (lambda (x) (list x x)) (forge:Run-atoms udt))))

; Checking Quantified variable
(printf "TEST 6 ~n~n")
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(@check-exn
 exn:fail?
 (lambda () 
   (lift-bounds-expr f-some-reaches-all '() udt)))

; Checking Set Comprehension constant case
(printf "TEST 7 ~n~n")
(define qvx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define f-set-comprehension (node/expr/comprehension empty-nodeinfo 1
                                  (list (cons qvx Node))
                                  (in qvx Node)))
(define uppers-set-comprehension (list (lift-bounds-expr Node '(qvx) udt)))

(@check-equal?
 (to-string (lift-bounds-expr f-set-comprehension '() udt))
 (to-string (map (lambda (ub) (apply append ub)) (apply cartesian-product uppers-set-comprehension))))


; Checking Set union case
(printf "TEST 8 ~n~n")
(define uppers-union (list node-bound (map (lambda (x) (list x x)) (forge:Run-atoms udt))))
(@check-equal?
 (to-string (lift-bounds-expr (+ Node univ) '() udt))
 (to-string (remove-duplicates (apply append uppers-union))))

; Checking Set minus case
(printf "TEST 9 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr (- Node univ) '() udt))
 (to-string node-bound))

; Checking Set intersection case
; TODO: In set minus, why are we just looking at the bounds of the LHS? 
(printf "TEST 10 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr (& Node (- Node (+ Node univ))) '() udt))
 (to-string node-bound))

; Checking Set Product case
(printf "TEST 11 ~n~n")
(define LHSProduct-bounds (lift-bounds-expr Node '() udt))
(define RHSProduct-bounds (lift-bounds-expr (-> Node univ) '() udt))
(define list-product-bounds (list LHSProduct-bounds RHSProduct-bounds))
(define product-map (map (lambda (ub) (apply append ub)) (apply cartesian-product list-product-bounds)))

(@check-equal?
 (to-string (lift-bounds-expr (-> Node (-> Node univ)) '() udt))
 (to-string product-map))

; Checking Set Join case
; Error case arity < 1
(printf "TEST 12 ~n~n")
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-expr (join Node Node) '() udt)))

; testing normal join case with two arguments  
(printf "TEST 13 ~n~n")
(define join-LHS (lift-bounds-expr edges '() udt))
(define join-RHS (lift-bounds-expr iden '() udt))
(define list-join (list join-LHS join-RHS))
(define newTuples (map (lambda (left-ub)
                         (map (lambda (right-ub)
                                (list left-ub right-ub))
                              (second list-join)))
                       (first list-join)))

(@check-equal?
 (to-string (lift-bounds-expr (join edges iden) '() udt))
 (to-string newTuples))


; Checking join with more than two arguments 
(printf "TEST 14 ~n~n")
(define join-LHS-more (lift-bounds-expr edges '() udt))
(define join-RHS-more (lift-bounds-expr iden '() udt))
(define list-join-more (list join-LHS join-RHS join-RHS))
(define newTuples-more (joinTuple list-join-more))
(define newTuples-further (foldl (lambda (curr acc) (joinTuple curr)) newTuples (rest (rest list-join-more))))

(@check-equal?
 (to-string (lift-bounds-expr (join edges iden iden) '() udt))
 (to-string newTuples-further))


; Checking Set transitive closure case
(printf "TEST 15~n~n")
(define transitive-closure-bounds (list (lift-bounds-expr edges '() udt)))
(@check-equal?
 (to-string (lift-bounds-expr (^ edges) '() udt))
 (to-string (buildClosureOfTupleSet transitive-closure-bounds)))

; Checking Set reflexive transitive closure case
(printf "TEST 16~n~n")
(define reflexive-transitive-closure-bounds (list (lift-bounds-expr edges '() udt)))
(define closureOfTupleSets (buildClosureOfTupleSet reflexive-transitive-closure-bounds))
(define appendedTuples (append closureOfTupleSets (map (lambda (x) (list x x)) (forge:Run-atoms udt))))
(@check-equal?
 (to-string (lift-bounds-expr (* edges) '() udt))
 (to-string appendedTuples))


; Checking Set transpose case
(printf "TEST 17 ~n~n")
(define transpose-bounds (list (lift-bounds-expr edges '() udt)))
 (@check-equal?
 (to-string (lift-bounds-expr (~ edges) '() udt))
 (to-string (map (lambda (x) (transposeTup x)) (first transpose-bounds))))

; Checking Set singleton case
(printf "TEST 18 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr (sing var-int-const-x) '() udt))
 (to-string int-bounds))

; Checking const int case
(printf "TEST 19~n~n")
(@check-equal?
 (to-string (lift-bounds-int var-int-const-x '() udt))
 (to-string int-bounds))

; Checking int with operator (should error)
(printf "TEST 20~n~n")
(define f-int-less (< var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-int f-int-less '() udt)))

(printf "TEST 21~n~n")
(define f-int-greater (> var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-int f-int-greater '() udt)))

; TODO: Checking sum "quantifier" case
;(printf "TEST 22 ~n~n")
;(define x (node/expr/quantifier-var empty-nodeinfo 1 'x))
;(define f-sum (node/int/sum-quant empty-nodeinfo
;                                  (list (cons x Node))
;                                  (node/int/op/card empty-nodeinfo (list (join edges x)))))
;(@check-equal?
; (to-string (lift-bounds-int f-sum '() udt))
; (to-string ()))


; TODO: Checking cardinality case -- stuck on bitwidth question 
; cardinality
;(printf "TEST 23 ~n~n")
;(define f-cardinality (node/int/op/card empty-nodeinfo (list Node)))
;(@check-equal?
; (to-string (lift-bounds-int f-cardinality '() udt))
; (to-string (node/int/op/card empty-nodeinfo (list edges))))
