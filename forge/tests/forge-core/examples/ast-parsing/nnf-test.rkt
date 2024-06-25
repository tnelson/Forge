#lang forge/core

(require forge/utils/to-nnf)
(require "nnf-test.frg")

(set-verbosity 0)

(define run-or-state 'run)

; Define the interpret-formula function call for each test predicate
(define basic-negation-result (interpret-formula run-or-state basic_negation '() '() '()))
(define double-negation-result (interpret-formula run-or-state double_negation '() '() '()))
(define no-negation-result (interpret-formula run-or-state no_negation '() '() '()))
(define implies-negation-result (interpret-formula run-or-state implies_negation '() '() '()))
(define not-and-negation-result (interpret-formula run-or-state not_and_negation '() '() '()))
(define quant-negation-result (interpret-formula run-or-state quant_negation '() '() '()))
(define exists-negation-result (interpret-formula run-or-state exists_negation '() '() '()))
(define complex-and-result (interpret-formula run-or-state complex_and '() '() '()))
(define complex-or-result (interpret-formula run-or-state complex_or '() '() '()))
(define complex-and-one-result (interpret-formula run-or-state complex_and_one '() '() '()))
(define quantifier-negation-all-result (interpret-formula run-or-state quantifier_negation_all '() '() '()))
(define quantifier-negation-some-result (interpret-formula run-or-state quantifier_negation_some '() '() '()))
(define quantifier-negation-one-result (interpret-formula run-or-state quantifier_negation_one '() '() '()))
(define quantifier-negation-lone-result (interpret-formula run-or-state quantifier_negation_lone '() '() '()))
(define logical-equivalence-result (interpret-formula run-or-state logical_equivalence '() '() '()))
(define logical-implies-result (interpret-formula run-or-state logical_implies '() '() '()))
(define iff-negation-inner-result (interpret-formula run-or-state iff_negation_inner '() '() '()))
(define iff-negation-outer-result (interpret-formula run-or-state iff_negation_outer '() '() '()))
(define overall-formula-result (interpret-formula run-or-state overall_formula '() '() '()))
(define expected-nnf-result (interpret-formula run-or-state expected_nnf '() '() '()))
(define complex-overall-formula-result (interpret-formula run-or-state complex_overall_formula '() '() '()))
(define complex-expected-nnf-result (interpret-formula run-or-state complex_expected_nnf '() '() '()))

; Define a function to check logical equivalence between two formulas
(define/contract (check-logical-equivalence formula1 formula2)
  (->* () (->* () (->* () boolean?)))
  (equal? formula1 formula2))

; Run the equivalence tests and print the results
(format "Basic Negation: ~a\n" basic-negation-result)
(format "Double Negation: ~a\n" double-negation-result)
(format "No Negation: ~a\n" no-negation-result)
(format "Implies Negation: ~a\n" implies-negation-result)
(format "Not And Negation: ~a\n" not-and-negation-result)
(format "Quant Negation: ~a\n" quant-negation-result)
(format "Exists Negation: ~a\n" exists-negation-result)
(format "Complex And One: ~a\n" complex-and-one-result)
(format "Complex And Two: ~a\n" complex-and-two-result)
(format "Complex Or One: ~a\n" complex-or-one-result)
(format "Complex Or Two: ~a\n" complex-or-two-result)
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

