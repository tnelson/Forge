#lang racket/base

(provide read-syntax)

(require
  racket/port
  forge/decode-wheat-lang/util
  (prefix-in f: forge/lang/reader))

(define TAB 9)

(define (read-syntax path port)
  (read-char port) ;; newline / tab
  (f:read-syntax path (filter-read-input-port port dread dpeek)))

(define (dread b* res)
  (decode-bytes! b* res)
  res)

(define (dpeek b* _i _evt res)
  (decode-bytes! b* res)
  res)

(define (decode-bytes! b* res)
  (when (and (exact-nonnegative-integer? res)
             (< 0 res))
    (for ((i (in-range res)))
      (bytes-set! b* i (decode-byte (bytes-ref b* i))))))

