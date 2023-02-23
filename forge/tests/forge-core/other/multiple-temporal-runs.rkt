#lang forge/core

; This file is a variant of multiple-runs.rkt that tests _temporal mode_.

; Check the behavior of multiple Run objects used simultaneously.
; Note that it's not enough to invoke the generators multiple times; we need
; to actually confirm they are producing appropriate instances and new instances.

(require (only-in rackunit check-eq? check-not-eq?))
(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)
(set-option! 'min_tracelength 2)

(sig A)
(sig B #:is-var #t)

(run sat-run-A
     #:preds [(some A) (always (no B))])
(run sat-run-B
     #:preds [(some B) (always (no A))])
(run unsat-run
     #:preds [(some A) (some B) (= A B)])

; Model-generator produces a stream
; In temporal mode, this needs to be either 'P (path for THIS configuration) or 'C (new configuration)
; We must use 'P for sat-run-B because there are no degrees of freedom in the configuration for that run (B is var)
(define sat-A-gen (forge:make-model-generator (forge:get-result sat-run-A) 'C))
(define sat-B-gen (forge:make-model-generator (forge:get-result sat-run-B) 'P))
(define unsat-gen (forge:make-model-generator (forge:get-result unsat-run) 'C))

; Get two instances; sat/unsat as expected
(define a1 (sat-A-gen))
(define b1 (sat-B-gen))
(check-eq? (Sat? a1) #t)
(check-eq? (Sat? b1) #t)
(check-eq? (Unsat? (unsat-gen)) #t)

; The first instance for each run has the appropriate number of A/B atoms
(check-eq? (length (hash-ref (first (Sat-instances a1)) 'B))
           0)
(check-eq? (length (hash-ref (first (Sat-instances b1)) 'A))
           0)

; get another two instances; sat/unsat as expected
(define a2 (sat-A-gen))
(define b2 (sat-B-gen))
(check-eq? (Sat? a2) #t)
(check-eq? (Sat? b2) #t)
(check-eq? (Unsat? (unsat-gen)) #t)

; The second instance for each run has the appropriate number of A/B atoms
(check-eq? (length (hash-ref (first (Sat-instances a2)) 'B))
           0)
(check-eq? (length (hash-ref (first (Sat-instances b2)) 'A))
           0)
; The first and second instances for each run are different (a1 != a2 etc.)
(check-not-eq? (first (Sat-instances a1)) (first (Sat-instances a2)))
(check-not-eq? (first (Sat-instances b1)) (first (Sat-instances b2)))

; Evaluation works and provides appropriate answers per run
(check-eq? (length (evaluate sat-run-A 'unused A))
           2)
(check-eq? (length (evaluate sat-run-A 'unused B))
           0)
(check-eq? (length (evaluate sat-run-B 'unused A))
           0)

; Not reliable in temporal mode due to above (B is var)
;(check-eq? (length (evaluate sat-run-B 'unused B))
;           2)


; Check that we can close a run
(forge:close-run sat-run-A)
; and still get (new) results from other runs
(define b3 (sat-B-gen))
(check-eq? (Sat? b3) #t)
(check-not-eq? (first (Sat-instances b3)) (first (Sat-instances b2)))
(check-not-eq? (first (Sat-instances b3)) (first (Sat-instances b1)))

