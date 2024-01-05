#lang racket/base

(provide set-value-a set-value-b
         get-value-a get-value-b)

(define value-a (box 0))
(define value-b 0)

(define (set-value-a x)
  (set! value-a (box x)))
(define (set-value-b x)
  (set! value-b x))

(define (get-value-a) (unbox value-a))
(define (get-value-b) value-b)
