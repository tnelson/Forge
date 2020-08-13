#lang racket

(require (for-syntax syntax/parse))
(require (prefix-in @ racket))

(provide (all-defined-out))

(define-syntax-rule (+ a b) (@+ a b))

(define-syntax (P stx)
  (syntax-parse stx #:datum-literals (+)
    [(P (+ a b))
     #'(printf "P (+ ~a ~a)~n" a b)]))