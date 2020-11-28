#lang forge/core

(require "lift-bounds.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require debug/repl)
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

; Checking atom case base case 
(printf "TEST 1 ~n~n")
(define sampleAtom (node/expr/atom empty-nodeinfo 1 Node))
(@check-equal?
 (to-string (lift-bounds-expr sampleAtom '() udt))
 (to-string (list (list (node/expr/atom-name sampleAtom)))))

; Checking relation name case base case 
(printf "TEST 2 ~n~n")
(define node-bound (list (list 'Node0) (list 'Node1) (list 'Node2) (list 'Node3) (list 'Node4) (list 'Node5) (list 'Node6)))
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


