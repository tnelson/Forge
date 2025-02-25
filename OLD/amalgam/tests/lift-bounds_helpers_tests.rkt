#lang forge/core
(require "../lift-bounds/lift-bounds_helpers.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))
(require rackunit/text-ui)

(run-tests
 (@test-suite
  " liftBoundsHelpers"
  (lambda () (display "Starting tests for liftBoundsHelpers"))
  (lambda () (display "All tests for liftBoundsHelpers passed!"))

  
  (@test-suite
   " transposeTup"
   (lambda () (display "Starting tests for transposeTup"))
   (lambda () (display "All tests for transposeTup passed!"))
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
                                       (transposeTup '(1 3))
                                       '(3 1)))

   (@check-exn
    exn:fail?
    (lambda () 
      (transposeTup '(3 1 2)))))

  (@test-suite
   " joinTuple"
   (lambda () (display "Starting tests for joinTuple"))
   (lambda () (display "All tests for joinTuple passed!"))
   (@test-case
    "TEST joinTuple on nothing to join on"
    (@check-equal?
     (joinTuple '((1 2) (3 4)) '((5 6)))
     '()))

   (@test-case
    "TEST joinTuple on one tuple to join in"
    (@check-equal?
     (joinTuple '((1 2) (3 4)) '((4 6)))
     '((3 6))))

   (@test-case
    "TEST joinTuple on 4 tuples to join in"
    (@check-equal?
     (joinTuple '((1 2) (3 4) (5 100)) '((4 6) (4 7) (4 9) (100 4)))
     '((3 6) (3 7) (3 9) (5 4)))))

  (@test-suite
   " buildClosureOfTupleSet"
   (lambda () (display "Starting tests for buildClosureOfTupleSet"))
   (lambda () (display "All tests for buildClosureOfTupleSet passed!"))
  
   (@test-case
    "TEST buildClosureOfTupleSet on empty tuple set"
    (@check-equal?
     (buildClosureOfTupleSet '())
     '()))

   (@test-case
    "TEST buildClosureOfTupleSet on two-joinable tuples"
    (@check-equal?
     (buildClosureOfTupleSet '((1 2) (2 4)))
     '((1 2) (2 4) (1 4))))


   (@test-case
    "TEST buildClosureOfTupleSet on three-joinable tuples"
    (@check-equal?
     (buildClosureOfTupleSet '((1 2) (2 4) (3 3)))
     '((1 2) (2 4) (3 3) (1 4)))))))
