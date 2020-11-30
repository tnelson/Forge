#lang forge/core
(require "../lift-bounds/lift-bounds_helpers.rkt")
(require "test_helpers.rkt")
(require (prefix-in @ rackunit))

;;;;;;;;;;;;;;;;; TRANSPOSE TUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "TEST 1 ~n~n")
(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup '())))

(printf "TEST 2~n~n")
(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup '(3))))

(printf "TEST 3~n~n")
(@check-equal?
 (to-string (transposeTup '(1 3)))
 (to-string '(3 1)))

(printf "TEST 4~n~n")
(@check-exn
 exn:fail?
 (lambda () 
   (transposeTup '(3 1 2))))

;;;;;;;;;;;;;;;;; JOIN TUPLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "TEST 5~n~n")
(@check-equal?
 (to-string (joinTuple '((1 2) (3 4)) '((5 6))))
 (to-string '()))


(printf "TEST 6~n~n")
(@check-equal?
 (to-string (joinTuple '((1 2) (3 4)) '((4 6))))
 (to-string '((3 6))))

(printf "TEST 7~n~n")
(@check-equal?
 (to-string (joinTuple '((1 2) (3 4) (5 100)) '((4 6) (4 7) (4 9) (100 4))))
 (to-string '((3 6) (3 7) (3 9) (5 4))))

;;;;;;;;;;;;;;;;; BUILD CLOSURE OF TUPLE SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "TEST 8~n~n")
(@check-equal?
 (to-string (buildClosureOfTupleSet '()))
 (to-string '()))

(printf "TEST 9~n~n")
(@check-equal?
 (to-string (buildClosureOfTupleSet '((1 2) (2 4))))
 (to-string '((1 2) (2 4) (1 4))))


(printf "TEST 10~n~n")
(@check-equal?
 (to-string (buildClosureOfTupleSet '((1 2) (2 4) (3 3))))
 (to-string '((1 2) (2 4) (3 3) (1 4))))
