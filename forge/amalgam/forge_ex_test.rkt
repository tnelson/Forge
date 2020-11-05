#lang forge/core

(require "forge_ex.rkt")    ; start by running this Forge spec
(require "tuple2Expr.rkt")  ; make available the tuple2Expr helpers

;----------------------------------------------
; Testing (TODO: move into Racket test module + rackunit)

; udt: run isUndirectedTree for 7 Node
; a run in forge/core doesn't automatically send to Sterling
(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)]) ; run macro does a lot, including binding udt identifier
;(display udt) ; this, if run, would send to Sterling

(define edgeTuple1 '(Node0 Node1))
(define edgeTuple2 '(Node1 Node2))

(tup2Expr edgeTuple1 udt) ; udt is in scope, because the name is def'd in the forge module

