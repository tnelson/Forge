#lang racket/base

; require shared.rkt via module, not code path
(require imports/shared)

; This is OK:
; require shared.rkt via code path
;(require "shared.rkt")

(provide other-print)

(define (other-print)
  (printf "~a ~a~n"
          (get-value-a)
          (get-value-b)))
  