#lang forge/core

; Check the behavior of multiple Run objects used simultaneously.
; Note that it's not enough to invoke the generators multiple times; we need
; to actually confirm they are producing appropriate instances and new instances.

(require (only-in rackunit check-eq? check-not-eq?))
(set-option! 'verbose 0)

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
(check-eq? (length (evaluate sat-run-B 'unused B))
           2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check that we can close a run without closing others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(forge:close-run sat-run-A)
; still get (new) results from other runs
(define b3 (sat-B-gen))
(check-eq? (Sat? b3) #t)
(check-not-eq? (first (Sat-instances b3)) (first (Sat-instances b2)))
(check-not-eq? (first (Sat-instances b3)) (first (Sat-instances b1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check behavior if we get a 2nd generator for the same run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Different generators re-start the model enumeration from the beginning, assuming
; the sequence of generator-arguments (e.g., 'next) is the same.
(define sat-B-gen-2 (forge:make-model-generator (forge:get-result sat-run-B) 'next))
(define b1-2 (sat-B-gen-2))
(define b2-2 (sat-B-gen-2))
(define b3-2 (sat-B-gen-2))
(define b4-2 (sat-B-gen-2))
(define b4 (sat-B-gen))

(check-eq? (first (Sat-instances b1)) (first (Sat-instances b1-2)))
(check-eq? (first (Sat-instances b2)) (first (Sat-instances b2-2)))
(check-eq? (first (Sat-instances b3)) (first (Sat-instances b3-2)))
(check-eq? (first (Sat-instances b4)) (first (Sat-instances b4-2)))
