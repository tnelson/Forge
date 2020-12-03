#lang forge/core
(require "../desugar/desugar.rkt")
(require "forge_ex.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])
; desugar-formula test cases

; constant formulas
(define const (node/formula/constant empty-nodeinfo Int))
#|(@test-case
 "TEST 1 constant formulas"
 (@check-equal?
  (to-string (desugar-formula const '() udt #t))
  (to-string const)))|#

; multiplicity formula
(define f-some-reaches-all (one edges))
(desugar-formula f-some-reaches-all '() udt #t)


