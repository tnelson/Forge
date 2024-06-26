#lang forge/core

(require forge/utils/to-nnf)
(require "nnf-test.frg")
(require (prefix-in @ (only-in rackunit check)))

(set-verbosity 0)

;; #lang forge is defined in /lang (expander, parser, etc.)
;;  This expands to Racket code using the macros in sigs.rkt.
;;  All of those are exposed in #lang forge/core.
;;  But _those_ invoke a functional library, that doesn't use macros heavily
;;    in sigs-functional.rkt.

; (all ([x (if foo Person Int)]) (= univ univ)) <-- "all" is a macro, defined in ast.rkt
;         ; ^ The macro is expanded before this is evaluated; this is JUST SYNTAX.
          ; ... So if the macro isn't VERY intricately defined, to handle this...
          ;; ... the pattern isn't matched. 

; (f (g 1) (h 2)) <-- call by value, inside-to-outside, standard Racket functions!

; Define a function to check logical equivalence between two formulas
(define (are-logically-equivalent/bounds? formula1 formula2)
  ;(run equiv-check-run #:preds [(! (iff formula1 formula2))])
  ; We could also just create these manually, and avoid the extra Run
  (define empty-run-spec (forge:Run-run-spec run-statement))
  (define empty-run-state (forge:Run-spec-state empty-run-spec))
  (define sigs (hash-values (forge:State-sigs empty-run-state)))
  (define relations (hash-values (forge:State-relations empty-run-state)))
  (define equiv-check-run
    ; The functional version needs to be told the sigs, relations etc. explicitly
    (make-run #:name (gensym)
              #:preds (list (! (iff formula1 formula2)))
              ; Int and succ are *added* by default by make-run; don't duplicate them.
              #:sigs (remove Int sigs)
              #:relations (remove succ relations)
              #:scope (forge:Run-spec-scope empty-run-spec)
              #:options (forge:State-options empty-run-state)))
  (is-unsat? equiv-check-run))

; Method 1: *syntactic* equivalence (not referential equality, but structural equality).
; Test: !!(some A) rewrites to [something equal? to] (some A)
; Problem: runs into the fact that two Q-vars aren't equal if created from separate locations.
; ^ This has the advantage of being faster, not invoking a solver or prover process, etc. 

; Method 2: *semantic equivalence* (up to a bound). 

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


; TODO: bring these back once we've fixed the comprehension case
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





; Run the equivalence tests and print the results
(format "Basic Negation: ~a\n" basic-negation-result)
(@check are-logically-equivalent/bounds? basic-negation-result basic_negation)
(format "Double Negation: ~a\n" double-negation-result)
(@check are-logically-equivalent/bounds? double-negation-result double_negation)
(format "No Negation: ~a\n" no-negation-result)
(@check are-logically-equivalent/bounds? no-negation-result no_negation)
(format "Implies Negation: ~a\n" implies-negation-result)
(@check are-logically-equivalent/bounds? implies-negation-result implies_negation)
(format "Not And Negation: ~a\n" not-and-negation-result)
(@check are-logically-equivalent/bounds? not-and-negation-result not_and_negation)
(format "Quant Negation: ~a\n" quant-negation-result)
(@check are-logically-equivalent/bounds? quant-negation-result quant_negation)
(format "Exists Negation: ~a\n" exists-negation-result)
(@check are-logically-equivalent/bounds? exists-negation-result exists_negation)
(format "Complex And: ~a\n" complex-and-result)
(@check are-logically-equivalent/bounds? complex-and-result complex_and)
(format "Complex Or: ~a\n" complex-or-result)
(@check are-logically-equivalent/bounds? complex-or-result complex_or)
(format "Quantifier Negation All: ~a\n" quantifier-negation-all-result)
(@check are-logically-equivalent/bounds? quantifier-negation-all-result quantifier_negation_all)
(format "Quantifier Negation Some: ~a\n" quantifier-negation-some-result)
(@check are-logically-equivalent/bounds? quantifier-negation-some-result quantifier_negation_some)
(format "Quantifier Negation One: ~a\n" quantifier-negation-one-result)
(@check are-logically-equivalent/bounds? quantifier-negation-one-result quantifier_negation_one)
(format "Quantifier Negation Lone: ~a\n" quantifier-negation-lone-result)
(@check are-logically-equivalent/bounds? quantifier-negation-lone-result quantifier_negation_lone)
(format "Logical Equivalence: ~a\n" logical-equivalence-result)
(@check are-logically-equivalent/bounds? logical-equivalence-result logical_equivalence)
(format "Logical Implies: ~a\n" logical-implies-result)
(@check are-logically-equivalent/bounds? logical-implies-result logical_implies)
(format "IFF Negation Inner: ~a\n" iff-negation-inner-result)
(@check are-logically-equivalent/bounds? iff-negation-inner-result iff_negation_inner)
(format "IFF Negation Outer: ~a\n" iff-negation-outer-result)
(@check are-logically-equivalent/bounds? iff-negation-outer-result iff_negation_outer)
(format "Overall Formula: ~a\n" overall-formula-result)
(@check are-logically-equivalent/bounds? overall-formula-result overall_formula)
(format "Expected NNF: ~a\n" expected-nnf-result)
(@check are-logically-equivalent/bounds? expected-nnf-result expected_nnf)

(format "Complex Overall Formula: ~a\n" complex-overall-formula-result)
(@check are-logically-equivalent/bounds? complex-overall-formula-result complex_overall_formula)
(format "Complex Expected NNF: ~a\n" complex-expected-nnf-result)
(@check are-logically-equivalent/bounds? complex-expected-nnf-result complex_expected_nnf)

; Check equivalence for the overall formula and expected NNF
;; (define test-nnf-equivalence (check-logical-equivalence overall-formula-result expected-nnf-result))
;; (format "NNF Equivalence: ~a\n" test-nnf-equivalence)
;; 
;; ; Check equivalence for the complex overall formula and expected NNF
;; (define complex-test-nnf-equivalence (check-logical-equivalence complex-overall-formula-result complex-expected-nnf-result))
;; (format "Complex NNF Equivalence: ~a\n" complex-test-nnf-equivalence)
