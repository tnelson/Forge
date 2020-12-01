#lang forge/core
(require "../lift-bounds/lift-bounds_helpers.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))

;;;;;;;;;;;;;;;;; TRANSPOSE TUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup '())))

(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup '(3))))

(@test-case
 "TEST transposeTup on valid tuple"(@check-equal?
 (to-string (transposeTup '(1 3)))
 (to-string '(3 1))))

(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup '(3 1 2))))

;;;;;;;;;;;;;;;;; JOIN TUPLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@test-case
 "TEST joinTuple on nothing to join on"
(@check-equal?
 (to-string (joinTuple '((1 2) (3 4)) '((5 6))))
 (to-string '())))

(@test-case
 "TEST joinTuple on one tuple to join in"
(@check-equal?
 (to-string (joinTuple '((1 2) (3 4)) '((4 6))))
 (to-string '((3 6)))))

(@test-case
 "TEST joinTuple on 4 tuples to join in"
(@check-equal?
 (to-string (joinTuple '((1 2) (3 4) (5 100)) '((4 6) (4 7) (4 9) (100 4))))
 (to-string '((3 6) (3 7) (3 9) (5 4)))))

;;;;;;;;;;;;;;;;; BUILD CLOSURE OF TUPLE SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@test-case
 "TEST buildClosureOfTupleSet on empty tuple set"
(@check-equal?
 (to-string (buildClosureOfTupleSet '()))
 (to-string '())))

(@test-case
 "TEST buildClosureOfTupleSet on two-joinable tuples"
(@check-equal?
 (to-string (buildClosureOfTupleSet '((1 2) (2 4))))
 (to-string '((1 2) (2 4) (1 4)))))


(@test-case
 "TEST buildClosureOfTupleSet on three-joinable tuples"
(@check-equal?
 (to-string (buildClosureOfTupleSet '((1 2) (2 4) (3 3))))
 (to-string '((1 2) (2 4) (3 3) (1 4)))))
