#lang racket

(require "sigs.rkt")

; Entry point for Amalgam from forge/core
;  This is a sketch for the moment

; E.g., at REPL (or in separate module that requires the forge module)
; > (require racket/stream racket/base forge/amalgam)
; > (build-provenances (cons '(Node3 Node1) "edges") (stream-first (forge:Run-result foo1)) foo1)

(provide build-provenances)

; pair<list<atom>, string>, hash<relation,list<list<atom>>>, Run -> provenance-set
(define (build-provenances tup inst a-run)
  (printf "build-provenances ~a ~a ~a ~n"
          tup inst
          (forge:Run-kodkod-bounds a-run))
  '())