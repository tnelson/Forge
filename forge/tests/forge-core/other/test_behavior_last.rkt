#lang forge/core

; Confirm that test runs are closed or kept open as expected. 
; (This is related to multiple_runs.rkt, but has a different focus.)

(require (only-in rackunit check-eq? check-not-eq? check-exn check-not-exn check-true))
(set-option! 'verbose 0)
(set-option! 'run_sterling 'off)

; Not the default:
(set-option! 'test_keep 'last) 

(sig A)
(sig B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modified option: 'test_keep is 'last
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This test should fail, but not immediately produce an exception:
(check-not-exn
 (lambda () (test sat-run-A
                  #:preds [(some A) (no B)]
                  #:expect unsat)))
; Same with this test. 
(check-not-exn
 (lambda () (test sat-run-B
                  #:preds [(some A) (no B)]
                  #:expect unsat)))

; This is not bound to an accessible identifier, but it _is_ kept in the state:
(define local-sat-run-A (hash-ref (forge:State-runmap forge:curr-state) 'sat-run-A))
(define local-sat-run-B (hash-ref (forge:State-runmap forge:curr-state) 'sat-run-B))
; Under the 'last option, the first run should have been closed
; DO NOT USE is-running?; that is for the solver process as a whole.
; Under the 'last option, the second run should remain open (since it is the last failure)
(check-true (forge:is-sat? local-sat-run-A))
(check-true (forge:is-run-closed? local-sat-run-A)) 
(check-true (forge:is-sat? local-sat-run-B))
(check-true (not (forge:is-run-closed? local-sat-run-B)))

; This is, however, not calling the failure-reporter that the Forge surface languages
; invoke at the end of their execution -- since this is forge/core.
; (output-all-test-failures)
