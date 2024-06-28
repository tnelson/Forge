#lang forge/core

(require forge/utils/to-skolem)
(require "skolem-test.frg")
(require (prefix-in @ (only-in rackunit check)))
(require (only-in racket remove-duplicates))

(set-verbosity 2)

(run run-statement #:preds [])

; specific to skolemization
(define (are-logically-equivalent/bounds? formula1 formula2 skolems)
  (define empty-run-spec (forge:Run-run-spec run-statement))
  (define empty-run-state (forge:Run-spec-state empty-run-spec))
  (define sigs (hash-values (forge:State-sigs empty-run-state)))
  (define relations (hash-values (forge:State-relations empty-run-state)))
  (define equiv-check-run
    (make-run #:name (gensym)
              #:preds (list (! (iff formula1 formula2)))
              #:sigs (remove Int sigs)
              #:relations (remove-duplicates (append (remove succ relations) skolems))
              #:scope (forge:Run-spec-scope empty-run-spec)
              #:options (forge:State-options empty-run-state)))
  (is-unsat? equiv-check-run))

(define first_pred quants)


(define-values (skolemized-fmla bounds) (interpret-formula run-statement first_pred '() '() '() '()))
(printf "skolemized-fmla: ~a\n" skolemized-fmla)
(printf "bounds: ~a\n" bounds)

(define skolems (map (lambda (b) (forge:bound-relation b)) bounds))
(are-logically-equivalent/bounds? first_pred skolemized-fmla skolems)


