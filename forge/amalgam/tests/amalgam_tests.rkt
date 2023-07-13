#lang forge/core
(require "../../amalgam.rkt")
(require "forge_ex.rkt")
(require (only-in rackunit test-suite))
(require rackunit/text-ui)

; Note: do not re-import everything. Instead, use only-in
;(require (prefix-in @ racket))


(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(run-tests
 (test-suite 
  " Desugar helpers tests"
  (lambda () (display "Starting tests for amalgam"))
  (lambda () (display "All tests for amalgam passed!"))

  ; Removed; function no longer exists
  ; union-product
  ;(@test-case
  ; (@check-equal?
  ;  (union-product (@set 1 2) (@set 1 2))
  ;  (@set '(1 2) '(2 2) '(2 1) '(1 1)))
   
  ))