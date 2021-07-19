#lang racket

(provide set-checker-hash! get-checker-hash)

(define checker-hash-box (box #f))

(define (get-checker-hash)
  (unbox checker-hash-box))

(define (set-checker-hash! h)
  (set-box! checker-hash-box h))