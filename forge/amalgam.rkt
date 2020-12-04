#lang racket

(require "sigs.rkt")
(require forge/amalgam/desugar/desugar)

; Entry point for Amalgam from forge/core
;  This is a sketch for the moment

; E.g., at REPL (or in separate module that requires the forge module)
; > (require racket/stream racket/base forge/amalgam)
; > (build-provenances (cons '(Node3 Node1) "edges") (stream-first (forge:Run-result foo1)) foo1)

(provide build-provenances)

; pair<list<atom>, string>, Run -> provenance-set
; Due to the way the evaluator works at the moment, this is always
; with respect to the current solver state for <a-run>.
(define (build-provenances tup a-run)
  (printf "build-provenances ~a ~a ~n"
          tup
          (forge:Run-kodkod-bounds a-run))
  ; get conjunction of predicates F from the run command


  ;(inst relations
  ;  (is otherA func)
  ;  (is otherB func))
  (inst l-alternate-inst
        [ (= edges
             (+ (-> (+ (-> N10 N20) 
                       (+ (-> N10 N30)
                          (+ (-> N20 N30)
                             (-> N30 N30)))) Red0)
                (-> (+ (-> N10 N10)
                       (+ (-> N10 N20)
                          (+ (-> N10 N30)
                             (+ (-> N20 N30)
                                (-> N30 N20))))) Green0)))])
  (run alternate
       #:preds []
       #:bounds l-alternate-inst)
       
  
  ; evaluate to make sure tup is locally necessary
  ; desugar F
  ; Pass in the run, not the bounds, since we may need more of the run (like atom-rels)
  ;(define kkbounds (forge:Run-kodkod-bounds a-run))
  ;(define desugared (desugar-formula F '() a-run))
  ; do amalgam descent on desugared F
  '())