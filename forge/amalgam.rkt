#lang racket

(require "sigs.rkt")
(require forge/amalgam/desugar)

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
  ; get conjunction of predicates F from the run command
  
  ; evaluate to make sure tup is locally necessary
  ; desugar F
  ; Pass in the run, not the bounds, since we may need more of the run (like atom-rels)
  ;(define kkbounds (forge:Run-kodkod-bounds a-run))
  ;(define desugared (desugar-formula F '() a-run))
  ; do amalgam descent on desugared F
  '())