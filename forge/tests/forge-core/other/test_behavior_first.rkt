#lang forge/core

; Confirm that test runs are closed or kept open as expected. 
; (This is related to multiple_runs.rkt, but has a different focus.)

(require (only-in rackunit check-eq? check-not-eq? check-exn check-not-exn check-true))
(set-option! 'verbose 0)
(set-option! 'run_sterling 'off)

; 'first is the default, so no need to run this line:
;(set-option! 'test_keep 'first) 

(sig A)
(sig B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Default option: 'test_keep is 'first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This test should fail and produce an exception immediately in 'first mode: 
(check-exn #rx"Failed test sat-run-A."
           (lambda () (test sat-run-A
                            #:preds [(some A) (no B)]
                            #:expect unsat)))

; This is not bound to an accessible identifier, but it _is_ kept in the state:
(define local-sat-run-A (hash-ref (forge:State-runmap forge:curr-state) 'sat-run-A))
; The test run should remain open, so that Sterling's evaluator is available, etc.
; DO NOT USE is-running?; that is for the solver process as a whole.
(check-true (forge:is-sat? local-sat-run-A))
(check-true (not (forge:is-run-closed? local-sat-run-A)))

;;;;;;;;;;;;;;;;;;;;;

; This test should pass, and its run should be closed immediately.
(check-not-exn 
 (lambda () (test sat-run-B
                  #:preds [(some A) (no B)]
                  #:expect sat)))
(define local-sat-run-B (hash-ref (forge:State-runmap forge:curr-state) 'sat-run-B))
(check-true (forge:is-sat? local-sat-run-B))
(check-true (forge:is-run-closed? local-sat-run-B))
