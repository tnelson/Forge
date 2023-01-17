#lang racket/base

(provide encode-byte decode-byte)

(define offset 13)
(define base 256)

(define (encode-byte bb)
  (rotate-byte bb +))

(define (decode-byte bb)
  (rotate-byte bb -))

(define (rotate-byte bb op)
  (modulo (op bb offset) base))

