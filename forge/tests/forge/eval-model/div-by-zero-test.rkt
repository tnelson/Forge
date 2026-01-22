#lang racket/base

;; Unit test for C1: Division by zero handling in eval-model.rkt
;;
;; Tests that eval-int-expr raises clear user errors for division/remainder
;; by zero, rather than crashing with raw Racket errors.

(require rackunit
         forge/server/eval-model
         (prefix-in ast: forge/lang/ast))

(define (make-int-constant n) (ast:int/func n))
(define (make-divide a b) (ast:divide/func a b))
(define (make-remainder a b) (ast:remainder/func a b))

(define test-bitwidth 4)
(define empty-binding (make-hash))

(define five (make-int-constant 5))
(define zero (make-int-constant 0))

(test-case
 "divide by zero raises forge error with context"
 (check-exn
  #rx"division by zero"
  (lambda ()
    (eval-int-expr (make-divide five zero) empty-binding test-bitwidth))))

(test-case
 "remainder by zero raises forge error with context"
 (check-exn
  #rx"remainder by zero"
  (lambda ()
    (eval-int-expr (make-remainder five zero) empty-binding test-bitwidth))))

(test-case
 "normal division works"
 (check-equal?
  (eval-int-expr (make-divide (make-int-constant 6) (make-int-constant 2))
                 empty-binding test-bitwidth)
  3))

(test-case
 "normal remainder works"
 (check-equal?
  (eval-int-expr (make-remainder (make-int-constant 7) (make-int-constant 3))
                 empty-binding test-bitwidth)
  1))

(test-case
 "negative division works"
 (check-equal?
  (eval-int-expr (make-divide (make-int-constant -6) (make-int-constant 2))
                 empty-binding test-bitwidth)
  -3))
