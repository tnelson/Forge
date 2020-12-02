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
(define f-some-reaches-all (no edges))
(desugar-formula f-some-reaches-all '() udt #t)

; quantified formula

; no
; QUESTION: THESE RETURN VERY LONG THINGS
#|(define no-test (no ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(define negated-formula  (node/formula/op/! empty-nodeinfo (list (in y (join x (^ edges))))))
(define new-quant-formula (node/formula/quantified empty-nodeinfo 'all (list [y Node]) negated-formula))
(@test-case
 "TEST NO formula curr-sign true"
 (@check-equal?
  (to-string (desugar-formula no-test '() udt #t))
  (to-string (desugar-formula new-quant-formula '() udt #t))))|#

; one
;(define one-test (one ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

; lone
;(define lone-test (lone ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

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
; QUESTION: SHOULD THIS BE CALLING DESUGAR-FORMULA-OP INSTEAD?
(define negation-test (! true))
(@test-case
 "TEST implies formula"
 (@check-equal?
  (to-string (desugar-formula negation-test '() udt #t))
  ;desugars to (not LHS) OR (RHS) 
  (to-string true)))

; INTEGER >
(define var-int-const-x (node/int/constant empty-nodeinfo 1))
(define var-int-const-y (node/int/constant empty-nodeinfo 2))
(define int-greater (> var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (desugar-formula int-greater '() udt)))

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

