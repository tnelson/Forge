#lang racket

(provide set-checker-hash! get-checker-hash set-ast-checker-hash! get-ast-checker-hash get-check-lang set-check-lang! get-inst-checker-hash set-inst-checker-hash!)

(define checker-hash-box (box #f))
(define ast-checker-hash-box (box #f))
(define inst-checker-hash-box (box #f))
(define check-lang-box (box #f))

(define (get-ast-checker-hash)
  (unbox ast-checker-hash-box))

(define (set-ast-checker-hash! h)
  (set-box! ast-checker-hash-box h))

(define (get-inst-checker-hash)
  (unbox inst-checker-hash-box))

(define (set-inst-checker-hash! h)
  (set-box! inst-checker-hash-box h))

(define (get-checker-hash)
  (unbox checker-hash-box))

(define (set-checker-hash! h)
  (set-box! checker-hash-box h))

(define (get-check-lang)
  (unbox check-lang-box))

(define (set-check-lang! h)
  (set-box! check-lang-box h))