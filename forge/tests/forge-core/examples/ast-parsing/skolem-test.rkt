#lang forge/core

(require forge/utils/to-skolem)
(require "skolem-test.frg")
(require (prefix-in @ (only-in rackunit check)))

(set-verbosity 2)

(run run-statement #:preds [])

(define (are-logically-equivalent/bounds? formula1 formula2)
  (define empty-run-spec (forge:Run-run-spec run-statement))
  (define empty-run-state (forge:Run-spec-state empty-run-spec))
  (define sigs (hash-values (forge:State-sigs empty-run-state)))
  (define relations (hash-values (forge:State-relations empty-run-state)))
  (define equiv-check-run
    (make-run #:name (gensym)
              #:preds (list (! (iff formula1 formula2)))
              #:sigs (remove Int sigs)
              #:relations (remove succ relations)
              #:scope (forge:Run-spec-scope empty-run-spec)
              #:options (forge:State-options empty-run-state)))
  (is-unsat? equiv-check-run))

(define first_pred quants)

(printf "skolemized-fmla: ~a~n" (interpret-formula run-statement first_pred '() '() '() '()))
; (@check are-logically-equivalent/bounds? skolemized-fmla quants_expected)


