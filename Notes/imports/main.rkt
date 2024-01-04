#lang racket/base

; require shared.rkt via code path
(require "shared.rkt")
(require "other.rkt")

; To see whether boxes are treated differently
(set-value-a 5)
(set-value-b 5)

(other-print) ; 0 0
(printf "~a ~a~n" (get-value-a) (get-value-b)) ; 5 5

; Since these print differently, it would seem that
;    "shared.rkt"
;    imports/shared
; are *separate modules*