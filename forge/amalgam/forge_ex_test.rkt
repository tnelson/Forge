#lang forge/core
(require debug/repl)
(require "forge_ex.rkt")    ; start by running this Forge spec
;(require "tuple2Expr.rkt")  ; make available the tuple2Expr helpers
(require "desugar.rkt")
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

;(printf "tup2Expr test ~n")
;(tup2Expr edgeTuple1 udt) ; udt is in scope, because the name is def'd in the forge module

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;(define Node  (declare-relation '(univ) 'univ "Node"))
;(define edges (declare-relation '(Node Node) 'Node "edges"))
(define f-symmetric (= edges (~ edges)))
(define f-irreflexive (no (& edges iden)))
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))

;(desugar-formula f-symmetric '() udt #t)

;(printf "Symmetric ~n")
;(desugar-formula f-symmetric '() udt true)
;(printf "Irreflexive ~n")
;(desugar-formula f-irreflexive '() udt)
;(printf "some-reaches-all ~n")
;(desugar-formula f-irreflexive '() udt true)
(desugar-formula f-symmetric '() udt true)




