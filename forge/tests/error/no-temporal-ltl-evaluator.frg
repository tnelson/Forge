#lang forge/core

; Confirm that trying to use temporal operators _in the evaluator_ without 
; being in temporal mode produces an error.

(require (only-in rackunit check-eq? check-not-eq?))
(set-option! 'verbose 0)

(sig A)

(run sat-run-A
     #:preds [(some A)])
(define sat-A-gen (forge:make-model-generator (forge:get-result sat-run-A) 'next))
(define a1 (sat-A-gen))

(evaluate sat-run-A 'unused (prime A))
