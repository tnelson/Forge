#lang racket

(provide set-checker-hash! get-checker-hash set-ast-checker-hash! get-ast-checker-hash)

(define checker-hash-box (box #f))
(define ast-checker-hash-box (box #f))

(define (get-ast-checker-hash)
  (unbox ast-checker-hash-box))

(define (set-ast-checker-hash! h)
  (set-box! ast-checker-hash-box h))

(define (get-checker-hash)
  (unbox checker-hash-box))

(define (set-checker-hash! h)
  (set-box! checker-hash-box h))