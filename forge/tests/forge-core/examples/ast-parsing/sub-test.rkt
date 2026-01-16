#lang forge/core

(require forge/utils/substitutor)
(require "sub-test.frg")
(require (prefix-in @ (only-in rackunit check)))

(set-verbosity 0)

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

(define quantified-for-quantified quantified_pre)
; variables need to be grabbed kind of manually

(define quantvars-1 (node/formula/quantified-decls
                      (node/formula/quantified-formula (node/fmla/pred-spacer-expanded quantified-for-quantified))))
(define var-to-sub-1-1 (car (car quantvars-1)))
(define var-to-sub-1-2 (car (car (cdr quantvars-1))))
(define quantified-for-quantified-result (substitute-formula run-statement quantified-for-quantified
                        '() '() '() var-to-sub-1-1 var-to-sub-1-2))
(@check are-logically-equivalent/bounds? quantified-for-quantified-result quantified_post)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define quantified-for-int int_pre)
(define var-to-sub-2-1 (car (car (node/formula/quantified-decls (node/fmla/pred-spacer-expanded int_pre)))))
; fetching 'a' for the join expr
(define quantvar-2 (car (car (node/formula/quantified-decls (node/fmla/pred-spacer-expanded int_pre)))))
; I guess this weird way of creating the relation works, since the test passes....
; This is helpful to now that rel is logically equivalent to Relation.
; TODO: what is the 'parent' field of a relation?
(define var-to-sub-2-2 (node/expr/op-on-exprs/join '() 2 (list quantvar-2 (node/expr/relation '() 1 'age (list 'Int) '() #f))))
(define quantified-for-int-result (substitute-formula run-statement quantified-for-int
                        '() '() '() var-to-sub-2-1 var-to-sub-2-2))
(@check are-logically-equivalent/bounds? quantified-for-int-result int_pre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
