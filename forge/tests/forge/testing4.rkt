#lang forge/core

(require "testing.rkt")
; (require "testing3.rkt")

(run my-run #:preds [acyclic (= edges (^ edges))])
(display my-run)


; (module cake racket
;         (require forge/core/main) 
;         (require "testing.rkt")
;         (sig X)
;         (run my-run #:preds [(no A) (one B)])
;         (display my-run)
;         (sig Y))
; (require 'cake)

; (run my-run)
; (display my-run)

; (with "testing.rkt"
;     (+ 1 2))