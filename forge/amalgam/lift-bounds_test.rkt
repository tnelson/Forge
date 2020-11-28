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
; TODO: Double check on this one how we are getting the bitwidth
#|(printf "TEST 3 ~n~n")
(define var-int-const-x (node/expr/constant empty-nodeinfo 1 'Int))
(@check-equal?
 (to-string (lift-bounds-expr var-int-const-x '() udt))
 (to-string (list (list -8) (list -7) (list -6) (list -5) (list -4) (list -3) (list -2) (list -1) (list 0) (list 1) (list 2) (list 3) (list 4) (list 5) (list 6) (list 7))))|#

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

; TODO: Checking Set Comprehension constant case 

; Checking Set union case
(printf "TEST 7 ~n~n")
(define uppers-union (list node-bound (map (lambda (x) (list x x)) (forge:Run-atoms udt))))
(@check-equal?
 (to-string (lift-bounds-expr (+ Node univ) '() udt))
 (to-string (remove-duplicates (apply append uppers-union))))

; Checking Set minus case
(printf "TEST 8 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr (- Node univ) '() udt))
 (to-string node-bound))

; TODO: Checking Set intersection case
; Q: In set minus, why are we just looking at the bounds of the LHS? 
(printf "TEST 9 ~n~n")
(@check-equal?
 (to-string (lift-bounds-expr (& Node (- Node (+ Node univ))) '() udt))
 (to-string node-bound))

; Checking Set Product case
(printf "TEST 10 ~n~n")
(define LHSProduct-bounds (lift-bounds-expr Node '() udt))
(define RHSProduct-bounds (lift-bounds-expr (-> Node univ) '() udt))
(define list-product-bounds (list LHSProduct-bounds RHSProduct-bounds))
(define product-map (map (lambda (ub) (apply append ub)) (apply cartesian-product list-product-bounds)))

(@check-equal?
 (to-string (lift-bounds-expr (-> Node (-> Node univ)) '() udt))
 (to-string product-map))

; Checking Set Join case
; Error case arity < 1
(printf "TEST 11 ~n~n")
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-expr (join Node Node) '() udt)))

; non-error case arity > 1
; TODO: Fix the check for arity because it is always evaluating to true -- waiting on email to Thomas  
#| (printf "TEST 12 ~n~n")
(define join-LHS (lift-bounds-expr iden '() udt))
(define join-RHS (lift-bounds-expr edges '() udt))
(define list-join (list join-LHS join-RHS))
(define currBinaryJoin (zip (first join-LHS) (second join-RHS))) 

(@check-equal?
 (to-string (lift-bounds-expr (join edges iden) '() udt))
 (to-string (foldl (lambda (curr acc) (zip acc curr)) currBinaryJoin (rest (rest list-join)))))|# 

; TODO (can't do yet): Checking Set transitive closure case

; TODO (can't do yet): Checking Set reflexive transitive closure case

; Checking Set transpose case
(printf "TEST 15 ~n~n")
(define transpose-bounds (list (lift-bounds-expr edges '() udt)))
 (@check-equal?
 (to-string (lift-bounds-expr (~ edges) '() udt))
 (to-string (map (lambda (x) (transposeTup x)) (first transpose-bounds))))

; (can't do yet) TODO: Checking Set singleton case
;(printf "TEST 16 ~n~n")
;(@check-equal?
; (to-string (lift-bounds-expr (sing var-int-const-x) '() udt))
; (to-string ))

; (can't do yet)  TODO: Checking const int case

; Checking int with operator (should error)
(printf "TEST 18~n~n")
(define var-int-const-x (node/int/constant empty-nodeinfo 1))
(define var-int-const-y (node/int/constant empty-nodeinfo 2))
(define f-int-less (< var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-int f-int-less '() udt)))

(printf "TEST 19~n~n")
(define f-int-greater (> var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
  (lift-bounds-int f-int-greater '() udt)))

; TODO: Checking sum "quantifier" case -- waiting on Tim's email 

; TODO: Checking cardinality case -- stuck on bitwidth question 
; cardinality
;(printf "TEST 21 ~n~n")
;(define f-cardinality (node/int/op/card empty-nodeinfo (list Node)))
;(@check-equal?
; (to-string (lift-bounds-int f-cardinality '() udt))
; (to-string (node/int/op/card empty-nodeinfo (list edges))))
