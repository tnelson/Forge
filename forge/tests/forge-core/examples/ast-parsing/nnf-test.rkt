#lang forge/core

(require forge/utils/to-nnf)
(require "nnf-test.frg")

(set-verbosity 0)

(run run-statement #:preds [])

; Define the interpret-formula function call for each test predicate
(define basic-negation-result (interpret-formula run-statement basic_negation '() '() '()))
(define double-negation-result (interpret-formula run-statement double_negation '() '() '()))
(define no-negation-result (interpret-formula run-statement no_negation '() '() '()))
(define implies-negation-result (interpret-formula run-statement implies_negation '() '() '()))
(define not-and-negation-result (interpret-formula run-statement not_and_negation '() '() '()))
(define quant-negation-result (interpret-formula run-statement quant_negation '() '() '()))
(define exists-negation-result (interpret-formula run-statement exists_negation '() '() '()))
(define complex-and-result (interpret-formula run-statement complex_and '() '() '()))
(define complex-or-result (interpret-formula run-statement complex_or '() '() '()))
(define quantifier-negation-all-result (interpret-formula run-statement quantifier_negation_all '() '() '()))
(define quantifier-negation-some-result (interpret-formula run-statement quantifier_negation_some '() '() '()))
(define quantifier-negation-one-result (interpret-formula run-statement quantifier_negation_one '() '() '()))
(define quantifier-negation-lone-result (interpret-formula run-statement quantifier_negation_lone '() '() '()))
(define logical-equivalence-result (interpret-formula run-statement logical_equivalence '() '() '()))
(define logical-implies-result (interpret-formula run-statement logical_implies '() '() '()))
(define iff-negation-inner-result (interpret-formula run-statement iff_negation_inner '() '() '()))
(define iff-negation-outer-result (interpret-formula run-statement iff_negation_outer '() '() '()))
(define overall-formula-result (interpret-formula run-statement overall_formula '() '() '()))
(define expected-nnf-result (interpret-formula run-statement expected_nnf '() '() '()))
(define complex-overall-formula-result (interpret-formula run-statement complex_overall_formula '() '() '()))
(define complex-expected-nnf-result (interpret-formula run-statement complex_expected_nnf '() '() '()))

; Define a function to check logical equivalence between two formulas
(define (check-logical-equivalence formula1 formula2)
  (equal? formula1 formula2))

; Run the equivalence tests and print the results
(format "Basic Negation: ~a\n" basic-negation-result)
(format "Double Negation: ~a\n" double-negation-result)
(format "No Negation: ~a\n" no-negation-result)
(format "Implies Negation: ~a\n" implies-negation-result)
(format "Not And Negation: ~a\n" not-and-negation-result)
(format "Quant Negation: ~a\n" quant-negation-result)
(format "Exists Negation: ~a\n" exists-negation-result)
(format "Complex And: ~a\n" complex-and-result)
(format "Complex Or: ~a\n" complex-or-result)
(format "Quantifier Negation All: ~a\n" quantifier-negation-all-result)
(format "Quantifier Negation Some: ~a\n" quantifier-negation-some-result)
(format "Quantifier Negation One: ~a\n" quantifier-negation-one-result)
(format "Quantifier Negation Lone: ~a\n" quantifier-negation-lone-result)
(format "Logical Equivalence: ~a\n" logical-equivalence-result)
(format "Logical Implies: ~a\n" logical-implies-result)
(format "IFF Negation Inner: ~a\n" iff-negation-inner-result)
(format "IFF Negation Outer: ~a\n" iff-negation-outer-result)
(format "Overall Formula: ~a\n" overall-formula-result)
(format "Expected NNF: ~a\n" expected-nnf-result)

; Check equivalence for the overall formula and expected NNF
(define test-nnf-equivalence (check-logical-equivalence overall-formula-result expected-nnf-result))
(format "NNF Equivalence: ~a\n" test-nnf-equivalence)

; Check equivalence for the complex overall formula and expected NNF
(define complex-test-nnf-equivalence (check-logical-equivalence complex-overall-formula-result complex-expected-nnf-result))
(format "Complex NNF Equivalence: ~a\n" complex-test-nnf-equivalence)

