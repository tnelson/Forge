#lang racket/base

(provide read-syntax)

(require
  racket/port
  forge/decode-wheat-lang/util
  (prefix-in f: forge/lang/reader))

;;; TODO secret prefix

(define (read-syntax path port)
  (f:read-syntax path (filter-read-input-port port rr pp)))

(define (rr bb res)
  (when (and (exact-nonnegative-integer? res)
           (< 0 res))
    (for ((i (in-range res)))
      (bytes-set! bb i (modulo (- (bytes-ref bb i) offset) 256))))
  res)

(define (pp bb ii evt res)
  (when (and (exact-nonnegative-integer? res)
           (< 0 res))
    (for ((i (in-range res)))
      (bytes-set! bb i (modulo (- (bytes-ref bb i) offset) 256))))
  res)

