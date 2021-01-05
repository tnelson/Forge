#lang forge/core
(require "../../amalgam.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require (prefix-in @ racket))
(require rackunit/text-ui)

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(run-tests (@test-suite 
            " Desugar helpers tests"
            (lambda () (display "Starting tests for amalgam"))
            (lambda () (display "All tests for amalgam passed!"))

          ; union-product
          (@test-case
           (@check-equal?
            (union-product (@set 1 2) (@set 1 2))
            (@set '(1 2) '(2 2) '(2 1) '(1 1)))
           )))