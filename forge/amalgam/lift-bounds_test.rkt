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
 (to-string (()