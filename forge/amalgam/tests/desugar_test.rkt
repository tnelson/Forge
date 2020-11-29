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
(@test-case
 "TEST 1 constant formulas"
 (@check-equal?
  (to-string (desugar-formula const '() udt #t))
  (to-string const)))

; multiplicity formula

; quantified formula

; no

; one

; lone

; desugar-formula-op test cases

; AND
(define and-test (and true false))
(@test-case
 "TEST AND formula curr-sign true"
 (@check-equal?
  (to-string (desugar-formula and-test '() udt #t))
  (to-string (node/formula/op/&& empty-nodeinfo (list true false)))))

(@test-case
 "TEST AND formula curr-sign false"
 (@check-equal?
  (to-string (desugar-formula and-test '() udt #f))
  (to-string (node/formula/op/|| empty-nodeinfo (list true false)))))

; OR
(define or-test (or true false))
(@test-case
 "TEST ORR formula curr-sign true"
 (@check-equal?
  (to-string (desugar-formula or-test '() udt #t))
  (to-string (node/formula/op/|| empty-nodeinfo (list true false)))))

(@test-case
 "TEST OR formula curr-sign false"
 (@check-equal?
  (to-string (desugar-formula or-test '() udt #f))
  (to-string (node/formula/op/&& empty-nodeinfo (list true false)))))

; IMPLIES
(define implies-test (implies true false))
(@test-case
 "TEST implies formula"
 (@check-equal?
  (to-string (desugar-formula implies-test '() udt #t))
  ;desugars to (not LHS) OR (RHS) 
  (to-string (node/formula/op/|| empty-nodeinfo (list true false)))))

; IN

; EQUALS

; NEGATION

; INTEGER >

; INTEGER <

; INTEGER =

; relation name

; atom

; Int constant

; other expression constants

; quantified variable

; set comprehension

; UNION

; SETMINUS

; INTERSECTION

; PRODUCT

; JOIN

; TRANSITIVE CLOSURE

; REFLEXIVE-TRANSITIVE CLOSURE

; TRANSPOSE

; SINGLETON

; CONSTANT INT

; sum

; desugar-int-op 
; int addition

