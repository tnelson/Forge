#lang forge/core

(require forge/utils/substitutor)
(require "sub-test.frg")

(set-verbosity 0)

(run run-statement #:preds [])

; pred sample_pet_substitution {
;     all a : Person | some b : Pet | b.owner = a
; }

(define test_fmla (node/formula/quantified-formula (car (node/formula/op-children 
                        (car (node/formula/op-children (node/fmla/pred-spacer-expanded sample_pet_substitution)))))))
(define foo (node/formula/quantified-decls test_fmla))
(printf "sample formula: ~a~n" test_fmla)
(printf "sample decls: ~a~n" (car (car foo)))

(format "Substituting b for c: ~a" (substitute-formula run-statement sample_pet_substitution 
                        '() '() '() (car (car foo)) (node/expr/atom '() 1 'c)))