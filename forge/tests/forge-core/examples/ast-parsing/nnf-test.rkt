#lang forge/core

(require forge/utils/to-nnf)
(require "nnf-test.frg")
(require (prefix-in @ (only-in rackunit check)))

(set-verbosity 0)

; Define a function to check logical equivalence between two formulas
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

(run run-statement #:preds [])

; Function to interpret formula and check logical equivalence
(define (perform-test/bounded test-case)
  (define formula (car test-case))
  (define description (cdr test-case))
  (define formula-result (interpret-formula run-statement formula '() '() '()))
  (printf "Running test: ~a\n" description)
  (@check are-logically-equivalent/bounds? formula-result formula)
  (printf "Test completed: ~a\n\n" description))

; List of test cases, pairing predicate names with descriptive test names
(define test-cases
  (list
    (cons basic_negation "Testing Basic Negation: Ensure no person has age 3")
    (cons double_negation "Testing Double Negation: Check double negation on age = 3")
    (cons no_negation "Testing No Negation: Verify that no person has a parent")
    (cons implies_negation "Testing Implies Negation: Ensure parent implications are correct")
    (cons not_and_negation "Testing Not And Negation: Ensure no two different people share the same parent")
    (cons quant_negation "Testing Quant Negation: Negate universal quantification over age 3")
    (cons exists_negation "Testing Exists Negation: Negate existential quantification over age 3")
    (cons complex_and "Testing Complex And: Complex AND condition on age with negations")
    (cons complex_or "Testing Complex Or: Complex OR condition on age with negations")
    (cons complex_and_or "Testing Complex And Or: Combination of AND and OR conditions on age")
    (cons quantifier_negation_all "Testing Quantifier Negation All: Negate 'all' quantifier condition on age")
    (cons quantifier_negation_some "Testing Quantifier Negation Some: Negate 'some' quantifier condition on age")
    (cons quantifier_negation_one "Testing Quantifier Negation One: Negate 'one' quantifier condition on age")
    (cons quantifier_negation_lone "Testing Quantifier Negation Lone: Negate 'lone' quantifier condition on age")
    (cons logical_equivalence "Testing Logical Equivalence: Test logical equivalence on age")
    (cons logical_implies "Testing Logical Implies: Test implications related to age and parent's age")
    (cons iff_negation_inner "Testing IFF Negation Inner: Inner negation within an iff statement on age")
    (cons iff_negation_outer "Testing IFF Negation Outer: Outer negation of an iff statement on age")
    (cons overall_formula "Testing Overall Formula: Verify the overall formula on negation of existence")
    (cons expected_nnf "Testing Expected NNF: Check if the NNF conversion holds for nonexistence on age")
    (cons complex_overall_formula "Testing Complex Overall Formula: Test complex conditions involving age and parent's age")
    (cons complex_expected_nnf "Testing Complex Expected NNF: Check if the NNF conversion holds for complex conditions")
  ))

; Run all test cases
(for-each perform-test/bounded test-cases)

