#lang forge/core

(require forge/utils/substitutor)
(require "sub-test.frg")

(set-verbosity 0)

(run run-statement #:preds [])

; substituting quantified variable nested within quantified expression
(define pred-1 sample_pet_substitution)
(define var-to-sub-1 (car (car (node/formula/quantified-decls (node/formula/quantified-formula (car (node/formula/op-children
                        (car (node/formula/op-children (node/fmla/pred-spacer-expanded pred-1))))))))))
(format "Substituting b for c: ~a" (substitute-formula run-statement pred-1 
                        '() '() '() var-to-sub-1 (node/expr/atom '() 1 'c)))
