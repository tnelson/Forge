#lang forge/core

; Check the behavior of multiple Run objects used simultaneously.
; Note that it's not enough to invoke the generators multiple times; we need
; to actually confirm they are producing appropriate instances and new instances.

(require (only-in rackunit check-eq? check-not-eq?))
;(set-option! 'verbose 0)
(set-option! 'verbose 5)

(sig A)
(sig B)

(run sat-run-A
     #:preds [(some A) (no B)])
(run sat-run-B
     #:preds [(some B) (no A)])
(run unsat-run
     #:preds [(some A) (some B) (= A B)])

; Model-generator produces a stream
(define sat-A-gen (forge:make-model-generator (forge:get-result sat-run-A) 'next))
(define sat-B-gen (forge:make-model-generator (forge:get-result sat-run-B) 'next))
(define unsat-gen (forge:make-model-generator (forge:get-result unsat-run) 'next))

(define a1 (sat-A-gen))
(define b1 (sat-B-gen))
(check-eq? (Sat? a1) #t)
(check-eq? (Sat? b1) #t)
(check-eq? (Unsat? (unsat-gen)) #t)
(check-eq? (length (hash-ref (first (Sat-instances a1)) 'B))
           0)
(check-eq? (length (hash-ref (first (Sat-instances b1)) 'A))
           0)
(define a2 (sat-A-gen))
(define b2 (sat-B-gen))
(check-eq? (Sat? a2) #t)
(check-eq? (Sat? b2) #t)
(check-eq? (Unsat? (unsat-gen)) #t)
(check-eq? (length (hash-ref (first (Sat-instances a2)) 'B))
           0)
(check-eq? (length (hash-ref (first (Sat-instances b2)) 'A))
           0)
(check-not-eq? (first (Sat-instances a1)) (first (Sat-instances a2)))
(check-not-eq? (first (Sat-instances b1)) (first (Sat-instances b2)))

